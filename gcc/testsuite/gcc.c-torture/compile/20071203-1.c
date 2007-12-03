/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

struct User { char username[10]; };

void
auth_set_username (struct User *user)
{
  char *d;
  char ch;
  d = user->username + (user->username[0] == '~');
  while ((ch = *d++) != '\0') /* do nothing */ ;
}
