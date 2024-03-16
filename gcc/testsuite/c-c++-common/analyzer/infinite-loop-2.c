extern int do_stuff (void);

/* Various misleading "while" loops that look like do-whiles due
   to proximity to another clause, but are actually empty.  */

void not_a_do_while_1 (int flag)
{
  if (flag) {
    do_stuff ();
    flag = 0;
  } while (flag); // TODO: should we complain here?
}

void not_a_do_while_2 (int flag)
{
  if (!flag) {
    do_stuff ();
    flag = 1;
  } while (flag); // TODO: should we complain here?
}

void not_a_do_while_3 (int flag)
{
  while (!flag) {
    flag = do_stuff ();
  } while (flag); // TODO: should we complain here? 
}

void not_a_do_while_4 (int flag)
{
  while (flag) {
    flag = do_stuff ();
  } while (flag); // TODO: should we complain here? 
}
