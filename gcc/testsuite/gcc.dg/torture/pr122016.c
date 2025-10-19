/* { dg-do compile } */
/* { dg-additional-options "-ftree-pre" } */

int merge_parse_args_argc;
char merge_parse_args_argv_0;
int strncmp(const char *, const char *, __SIZE_TYPE__);
void _setjmp(void);
typedef enum { FALSE, TRUE } boool;
void directory_exists(void);
void merge_parse_args(void) {
  int i;
  boool help_found = FALSE;
  while (i < merge_parse_args_argc && !help_found) {
    if (strncmp("", &merge_parse_args_argv_0, 2))
      help_found = TRUE;
    else {
      for (;;) {
        _setjmp();
        break;
      }
      i++;
      directory_exists();
    }
    i++;
  }
  _setjmp();
}
