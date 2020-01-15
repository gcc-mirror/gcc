/* Test inline functions declared in inner scopes.  Bug 93072.  */
/* { dg-do compile } */
/* { dg-options "" } */

void
inline_1 (void)
{
}

void
inline_2 (void)
{
}

static void
inline_static_1 (void)
{
}

static void
inline_static_2 (void)
{
}

static void
test (void)
{
  inline void inline_1 (void);
  if (inline_1 == 0) ;
  extern inline void inline_2 (void);
  if (inline_2 == 0) ;
  inline void inline_3 (void);
  if (inline_3 == 0) ;
  extern inline void inline_4 (void);
  if (inline_4 == 0) ;
  inline void inline_static_1 (void);
  if (inline_static_1 == 0) ;
  extern inline void inline_static_2 (void);
  if (inline_static_2 == 0) ;
}

void
inline_3 (void)
{
}

void
inline_4 (void)
{
}
