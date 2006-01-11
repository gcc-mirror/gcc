/* This test exposed a bug in combine where it was improperly changing
   the mode of a register.  The bug appeared to be latent until web
   was moved after combine.  This is the reduced test that fails 
   by crashing in reload.  */


typedef struct cpp_reader cpp_reader;
typedef struct cpp_string cpp_string;
struct cpp_string
{
  unsigned int len;
  const unsigned char *text;
};
struct cpp_callbacks
{
  void (*ident) (cpp_reader *, unsigned int, const cpp_string *);
};
static void cb_ident (cpp_reader *, unsigned int, const cpp_string *);
init_c_lex (void)
{
  struct cpp_callbacks *cb;
  cb->ident = cb_ident;
}
cb_ident (cpp_reader * pfile __attribute__ ((__unused__)), unsigned int
line
          __attribute__ ((__unused__)), const cpp_string * str
          __attribute__ ((__unused__)))
{
  {
    cpp_string cstr = {
    };
    if (cpp_interpret_string (pfile, str, 1, &cstr, 0))
      {
      }
  }
}
