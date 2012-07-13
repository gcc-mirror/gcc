/* { dg-do run } */
/* SEGV at comment below.  */
typedef unsigned int size_t;
typedef enum har {
  he_fatal = (-199),
  he_not_initialized,
  he_bad_input,
  he_memory_too_small,
  he_bad_action,
  he_duplicate,
  he_bad_nonce,
  he_stale_nonce,
  he_bad_credentials,
  he_bad_user,
  he_no_such_user,
  he_bad_passwd,
  he_unknown_auth_scheme,
  he_not_found,
  he_failed_digest_file_check,
  he_failed_digest_file_save,
  he_process_not_privileged,
  he_other,
  he_end_of_range,
  ha_no_error = 0,
  ha_no_value = 1
} har;
typedef enum realm_type
{
  axis_realm = 0,
  ws_realm
} realm_type;

__attribute__((__noclone__, __noinline__))
har has_www_auth(char *, size_t, realm_type, har);

__attribute__((__noclone__, __noinline__))
har has_auth_user(const char *, const char *, realm_type, char *, size_t);

__attribute__((__noclone__, __noinline__))
char *ha_get_string_value(void);

typedef struct
{
  unsigned int track_id;
  char* user;
  char* realm;
  char* authent;
  int internal_realm;
} request;
enum user_response {
  file_not_found_user_response = -3,
  access_denied_user_response = -2,
  no_user_response = -1,
  ok_user_response = 0
};
struct realm_group {
  char *name;
  int id;
  struct realm_group *next;
};
struct realm {
  char *name;
  char *space;
  struct realm_group *groups;
  struct realm *next;
};
struct user_info {
  char *name;
  int no_groups;
  int groups[128];
  struct user_info *next;
};
static struct user_info *find_user(const char *user_name);
static int is_member_of_groups(const struct user_info *user_item,
                                const struct realm_group *groups);
int authent_author(request *req);
struct realm *realms = ((void *)0);
struct user_info *users = ((void *)0);
static struct user_info*
find_user(const char *user_name)
{
  struct user_info *user_item;
  user_item = users;
  while (user_item != ((void *)0)) {
    /* SEGV due to NULL access here on user_name.  See also comment below.  */
    if ((__builtin_strcmp(user_item->name, user_name) == 0))
      break;
    user_item = user_item->next;
  }
  return user_item;
}
static int
is_member_of_groups(const struct user_info *user_item,
                    const struct realm_group *groups)
{
  const struct realm_group *group_item;
  int i;
  group_item = groups;
  while (group_item != ((void *)0)) {
    for (i = 0; i < user_item->no_groups; i++)
      if (user_item->groups[i] == group_item->id)
	return 0;
    group_item = group_item->next;
  }
  return -1;
}
char *foo (void) __attribute__((__noclone__, __noinline__));
char* g_strdup (const char *str) __attribute__((__malloc__, __noclone__, __noinline__));
int g_strcmp0 (const char *str1, const char *str2);
static int
is_basic(char **user)
{
  char *passwd_ptr;
  char *authent = foo();
  passwd_ptr = __builtin_strchr(authent, ':');
  if (passwd_ptr != ((void *)0)) {
    *user = g_strdup(authent);
    return 0;
  }
  return -1;
}
static int
is_digest(char **user)
{
  int ret_val = -1;
  char *authent;
  authent = ha_get_string_value();
  if (authent) {
    *user = g_strdup(authent);
    ret_val = 0;
  }
  return ret_val;
}
__attribute__((__noclone__, __noinline__))
void g_free (void * mem);
static enum user_response
get_user_info_from_header(const realm_type type,
                          char **user_name,
                          struct user_info **user_item)
{
  int ret_val = no_user_response;
  if ((type == ws_realm)) {
    if (is_basic(user_name) == 0)
      ret_val = access_denied_user_response;
    if (is_digest(user_name) == 0)
      ret_val = ok_user_response;
  } else {
    if (is_basic(user_name) < 0 &&
	/* Load of *user_name here, but not after the is_digest call.  */
	is_digest(user_name) < 0)
      ;
    else if ((*user_item = find_user(*user_name)) != ((void *)0))
      ret_val = ok_user_response;
    else
      ret_val = access_denied_user_response;
    if (ret_val != ok_user_response)
      g_free(*user_name);
  }
  return ret_val;
}
static enum user_response
authenticate_user(request *req,
                  char **user_name,
                  struct user_info **user_item)
{
  char *authent = ((void *)0);
  har resp = ha_no_value;
  enum user_response user_resp;
  int ret_val = no_user_response;
  if (req->authent && __builtin_strlen(req->authent)) {
    authent = req->authent;
    user_resp = get_user_info_from_header(req->internal_realm,
                                          user_name,
                                          user_item);
    if (user_resp == ok_user_response) {
      resp = has_auth_user(authent, 0, req->internal_realm, "", 1);
      if (resp == ha_no_error)
	ret_val = ok_user_response;
      else if (resp != he_stale_nonce)
	ret_val = access_denied_user_response;
    } else if (user_resp == access_denied_user_response)
      ret_val = access_denied_user_response;
  }
  if (resp != he_memory_too_small && resp != ha_no_error)
    resp = has_www_auth("", 1, req->internal_realm, resp);
  return ret_val;
}

int __attribute__ ((__noinline__, __noclone__))
authent_author(request *req)
{
  struct realm *realm;
  char *user_name = ((void *)0);
  struct user_info *user_item = ((void *)0);
  int res = 0;
  asm ("");
  realm = realms;
  if (__builtin_strcmp("Wsd", realm->name) == 0) {
    req->internal_realm = ws_realm;
    is_digest(&user_name);
  }
  if (authenticate_user(req, &user_name, &user_item) < 0) {
    if (user_name != ((void *)0))
      req->user = user_name;
    res = -2;
    goto authent_author_return;
  }
  if (is_member_of_groups(user_item, realm->groups) < 0)
    res = -1;
authent_author_return:
  return res;
}

int good0, good1, good2;

__attribute__ ((__noinline__, __noclone__))
char *foo(void)
{
  asm ("");
  good0++;
  return "";
}

__attribute__ ((__noinline__, __noclone__))
char *ha_get_string_value(void)
{
  asm ("");
  good1++;
  return "f";
}

__attribute__ ((__noinline__, __noclone__))
har has_auth_user(const char *a, const char *b, realm_type c, char *d, size_t e)
{
  asm ("");
  if (*a != 'z' || a[1] != 0 || b != 0 || c != axis_realm || *d != 0
      || e != 1)
    __builtin_abort ();
  return ha_no_error;
}

__attribute__ ((__noinline__, __noclone__))
har has_www_auth(char *a, size_t b, realm_type c, har d)
{
  (void)(*a+b+c+d);
  asm ("");
  __builtin_abort ();
}


char *strdupped_user = "me";
__attribute__((__malloc__, __noclone__, __noinline__))
char* g_strdup (const char *str)
{
  asm ("");
  if (*str != 'f')
    __builtin_abort ();
  good2++;
  return strdupped_user;
}

__attribute__((__noclone__, __noinline__))
void g_free (void * mem)
{
  (void)mem;
  asm ("");
  __builtin_abort ();
}

struct user_info me = { .name = "me", .no_groups = 1, .groups = {42}, .next = 0};
struct user_info you = { .name = "you", .next = &me};
struct realm_group xgroups = { .name = "*", .id = 42, .next = 0};

int main(void)
{
  char *orig_user = "?";
  struct realm r = { .name = "x", .space = "space?", .groups = &xgroups, .next = 0};
  request req = { .user = orig_user, .realm = "!", .authent = "z",
		  .internal_realm = axis_realm};
  realms = &r;
  users = &you;
  if (authent_author (&req) != 0 || good0 != 1 || good1 != 1 || good2 != 1
      || req.user != orig_user
      || req.internal_realm != axis_realm)
    __builtin_abort ();
  __builtin_exit (0);
}

