/* PR 11184 */
/* Origin:  Dara Hazeghi <dhazeghi@yahoo.com> */

void *
objc_msg_sendv (char * arg_frame, void (*foo)())
{
  return __builtin_apply ( foo, arg_frame, 4);
}

