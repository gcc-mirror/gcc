/* { dg-do compile } */
/* { dg-additional-options "-ftree-pre" } */

int merge_parse_args_argc;
char merge_parse_args_argv_0;
int strncmp(char *, char *, long);
void _setjmp();
typedef enum { FALSE, TRUE } boool;
void directory_exists();
void merge_parse_args() {
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
