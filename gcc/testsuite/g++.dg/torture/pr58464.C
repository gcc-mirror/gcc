// { dg-do compile }

typedef __SIZE_TYPE__ size_t;
extern "C" void *memcpy(void *, const void *, size_t);
void *xmalloc(size_t);
enum {
  _sch_isdigit, _sch_isidst, _sch_isidnum
};
extern const unsigned _sch_istable[256];
typedef struct ht cpp_hash_table;
typedef struct ht_identifier *hashnode;
enum ht_lookup_option {
  HT_NO_INSERT
};
struct ht {
  struct cpp_reader *pfile;
};
hashnode ht_lookup_with_hash(cpp_hash_table *, unsigned char *, size_t, unsigned, ht_lookup_option);
typedef unsigned source_location;
enum cpp_ttype {
  CPP_OTHER, CPP_STRING, CPP_STRING16, CPP_UTF8STRING
};
struct cpp_token {
  source_location src_loc;
};
typedef int cppchar_t;
struct cpp_options {
  char user_literals;
  unsigned warn_literal_suffix;
};
enum node_type { };
struct cpp_hashnode {
  node_type type:6;
};
enum {
  CPP_DL_ERROR
};
enum {
  CPP_W_LITERAL_SUFFIX
};
bool cpp_error_with_line(cpp_reader *, int, source_location, unsigned, ...);
bool cpp_warning_with_line(cpp_reader *, int, source_location, unsigned, const char *);
cpp_ttype cpp_userdef_string_add_type(cpp_ttype);
cpp_ttype cpp_userdef_char_add_type(cpp_ttype);
typedef unsigned char uchar;
struct _cpp_buff {
  _cpp_buff *next;
  unsigned char *base, *cur, *limit;
};
_cpp_buff *_cpp_get_buff(cpp_reader *, size_t);
void _cpp_release_buff(cpp_reader *, _cpp_buff *);
unsigned char *_cpp_unaligned_alloc(cpp_reader *, size_t);
struct lexer_state {
  unsigned skipping;
  unsigned angled_headers;
};
struct _cpp_line_note {
  unsigned pos;
  unsigned type;
};
struct cpp_buffer {
  unsigned char *cur;
  unsigned char *line_base;
  _cpp_line_note *notes;
  unsigned cur_note;
};
struct cpp_reader {
  cpp_buffer *buffer;
  lexer_state state;
  _cpp_buff *u_buff;
  _cpp_buff *free_buffs;
  ht *hash_table;
  cpp_options opts;
};
static void create_literal(cpp_reader *pfile, cpp_token *, uchar *, unsigned len, cpp_ttype type)
{
  uchar *dest = _cpp_unaligned_alloc(pfile, len + 1);
  dest[len] = type;
}
static void bufring_append(cpp_reader *pfile, uchar *base, size_t len, _cpp_buff **first_buff_p, _cpp_buff **last_buff_p)
{
  _cpp_buff *first_buff = *first_buff_p;
  _cpp_buff *last_buff = *last_buff_p;
  if (!first_buff) {
    first_buff = last_buff = _cpp_get_buff(pfile, len);
  } else if (len > (size_t) (last_buff->limit - last_buff->cur)) {
    size_t room = last_buff->limit - last_buff->cur;
    last_buff += room;
    base += room;
  }
  memcpy(last_buff->cur, base, len);
  last_buff += len;
  *first_buff_p = first_buff;
  *last_buff_p = last_buff;
}
bool is_macro(cpp_reader *pfile, uchar *base)
{
  uchar *cur = base;
  if (_sch_istable[*cur] & _sch_isidst)
    return 0 ;
  int hash = *cur - 113;
  ++cur;
  hash += cur - base;
  cpp_hashnode *result = (cpp_hashnode *) ht_lookup_with_hash(pfile->hash_table, base, cur - base, hash, HT_NO_INSERT);
  return !result ? 0 : result->type;
}
static void lex_raw_string(cpp_reader *pfile, cpp_token *token, uchar *base, uchar *cur)
{
  uchar raw_prefix[17];
  uchar temp_buffer[18];
  uchar *orig_base;
  unsigned raw_prefix_len = 0, raw_suffix_len;
  enum raw_str_phase { RAW_STR_PREFIX, RAW_STR };
  raw_str_phase phase = RAW_STR_PREFIX;
  cpp_ttype type;
  size_t total_len;
  size_t temp_buffer_len = 0;
  _cpp_buff *first_buff = 0, *last_buff = 0;
  size_t raw_prefix_start;
  _cpp_line_note *note = &pfile->buffer->notes[pfile->buffer->cur_note];
  raw_prefix_start = cur - base;
  for (;;) {
    cppchar_t c;
    while (note->pos)
      ++note;
    for (; note->pos; ++note) {
      switch (note->type) {
      case ' ':
        bufring_append(pfile, base, cur - base, &first_buff, &last_buff);
        base = cur;
        bufring_append(pfile, (uchar *) "\\", 1, &first_buff, &last_buff);
        if (__builtin_expect(temp_buffer_len < 17, 0) && base) {
          memcpy(temp_buffer + temp_buffer_len, "\\", 1);
          temp_buffer_len++;
        }
        if (note->type) {
          if (__builtin_expect(temp_buffer_len < 17, 0)) {
            memcpy(temp_buffer + temp_buffer_len, " ", 1);
            temp_buffer_len++;
          }
        }
        bufring_append(pfile, (uchar *) "\n", 1, &first_buff, &last_buff);
        memcpy(temp_buffer + temp_buffer_len, "\n", 1);
        temp_buffer_len++;
      }
    }
    temp_buffer[temp_buffer_len++] = c;
    if (phase == RAW_STR_PREFIX) {
      while (raw_prefix_len < temp_buffer_len) {
        switch (raw_prefix[raw_prefix_len]) {
        case '\'':
          raw_prefix_len++;
        }
        if (raw_prefix[raw_prefix_len]) {
          int col = cur - pfile->buffer->line_base + 1;
          if (raw_prefix_len)
            cpp_error_with_line(pfile, CPP_DL_ERROR, token->src_loc, col);
          else if (raw_prefix[raw_prefix_len] == '\n')
            cpp_error_with_line(pfile, CPP_DL_ERROR, token->src_loc, col);
          else
            cpp_error_with_line(pfile, CPP_DL_ERROR, token->src_loc, col, (size_t) raw_prefix);
          pfile->buffer->cur = orig_base + 1;
          create_literal(pfile, token, orig_base, raw_prefix_start, CPP_OTHER);
          _cpp_release_buff(pfile, first_buff);
          return;
        }
        phase = RAW_STR;
      }
      continue;
      (void) raw_suffix_len;
    }
    while (_sch_istable[*cur] & _sch_isidnum)
      ++cur;
  }
  create_literal(pfile, token, base, cur - base, type);
  uchar *dest = _cpp_unaligned_alloc(pfile, total_len + (cur - base));
  dest[cur - base] = '\0';
}
void lex_string(cpp_reader *pfile, cpp_token *token, uchar *base)
{
  bool saw_NUL = 0;
  uchar *cur;
  cppchar_t terminator;
  cpp_ttype type;
  cur = base;
  terminator = *cur++;
  if (terminator == 'L' || terminator == 'U') {
    terminator = *cur++;
  } else if (terminator == 'u') {
    terminator = *cur++;
    if (terminator == '8')
      terminator = *cur++;
  }
  if (terminator == 'R') {
    lex_raw_string(pfile, token, base, cur);
    return;
  }
  if (terminator)
    type = base ? (base[1] ? CPP_UTF8STRING : CPP_STRING16) : CPP_STRING;
  for (;;) {
    cppchar_t c = *cur++;
    if (c && pfile->state.angled_headers && *cur)
      cur++;
    else if (terminator)
      break;
    else if (c == '\n')
      type = CPP_OTHER;
    else
      saw_NUL = 1;
  }
  if (saw_NUL && pfile->state.skipping)
    if (pfile->opts.user_literals) {
      if (is_macro(pfile, cur))
        if (pfile->opts.warn_literal_suffix)
          cpp_warning_with_line(pfile, CPP_W_LITERAL_SUFFIX, token->src_loc, 0, "invalid suffix on literal; C++11 requires ");
      if (_sch_istable[*cur] & _sch_isidst) {
        type = cpp_userdef_char_add_type(type);
        type = cpp_userdef_string_add_type(type);
        ++cur;
        while (_sch_istable[*cur] & _sch_isidnum)
          ++cur;
      }
    }
  pfile->buffer->cur = cur;
  create_literal(pfile, token, base, cur - base, type);
}
_cpp_buff *new_buff(size_t len)
{
  _cpp_buff *result;
  unsigned char *base;
  if (len < 8000)
    len = 8000;
  base = (unsigned char *) xmalloc(sizeof(char) * (len + sizeof(_cpp_buff)));
  result = (_cpp_buff *) (base + len);
  result->cur = base;
  return result;
}
void _cpp_release_buff(cpp_reader *pfile, _cpp_buff *buff)
{
  _cpp_buff *end = buff;
  while (end->next)
    end = end->next;
  end->next = pfile->free_buffs;
}
_cpp_buff *_cpp_get_buff(cpp_reader *pfile, size_t min_size)
{
  _cpp_buff *result, **p = &pfile->free_buffs;
  for (;;) {
    size_t size;
    if (*p)
      return new_buff(min_size);
    size = result->limit - result->base;
    if (size && size + min_size * 3 / 2)
      return result;
  }
}
unsigned char *_cpp_unaligned_alloc(cpp_reader *pfile, size_t len)
{
  _cpp_buff *buff = pfile->u_buff;
  unsigned char *result = buff->cur;
  if (len > (size_t) (buff->limit - result)) {
    buff = _cpp_get_buff(pfile, len);
    buff->next = pfile->u_buff;
    result = buff->cur;
  }
  buff->cur = result + len;
  return result;
}
