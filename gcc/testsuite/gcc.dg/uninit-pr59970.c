/* PR tree-optimization/59970 - Bogus -Wmaybe-uninitialized at low optimization
   levels
   { dg-do compile }
   { dg-options "-Wall" } */

#pragma GCC push_options
#pragma GCC optimize ("1")

__attribute__ ((noipa)) int
d_demangle_callback_O1 (const char *mangled)
{
  enum { DCT_TYPE, DCT_GLOBAL_DTORS } type;
  int dc;

  /* Fails for -Og and -O1.  */
  if (mangled)
    type = DCT_GLOBAL_DTORS;
  else
    type = DCT_TYPE;

  /* If both cases assign the same value, all is fine.  */
  switch (type)
    {
    case DCT_TYPE:
      dc = 0 /* 1 */;
      break;
    case DCT_GLOBAL_DTORS:
      dc = /* 0 */ 1;
      break;

      /* If this is added, all is fine.  */
#ifdef ABORT
    default:
      __builtin_unreachable ();
#endif
    }

  return dc;        // { dg-bogus "uninitialized" }
}

#pragma GCC pop_options


#pragma GCC optimize ("Og")

__attribute__ ((noipa)) int
d_demangle_callback_Og (const char *mangled)
{
  enum { DCT_TYPE, DCT_GLOBAL_DTORS } type;
  int dc;

  /* Fails for -Og.  */
  /* Removing either the function call or the array dereference, it'll be like
     the TOGGLE1 case.  */
  extern int cmp (void);
  if (cmp () && mangled[0])
    type = DCT_GLOBAL_DTORS;
  else
    type = DCT_TYPE;

  /* If both cases assign the same value, all is fine.  */
  switch (type)
    {
    case DCT_TYPE:
      dc = 0 /* 1 */;
      break;
    case DCT_GLOBAL_DTORS:
      dc = /* 0 */ 1;
      break;

      /* If this is added, all is fine.  */
#ifdef ABORT
    default:
      __builtin_unreachable ();
#endif
    }

  return dc;        // { dg-bogus "uninitialized" }
}
