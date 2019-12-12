/* Test inline functions declared in inner scopes.  Bugs 88720 and 88726.  */
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

static void inline_static_3 (void);
static void inline_static_4 (void);

static void
test (void)
{
  inline void inline_1 (void);
  extern inline void inline_2 (void);
  inline void inline_3 (void);
  extern inline void inline_4 (void);
  inline void inline_static_1 (void);
  extern inline void inline_static_2 (void);
  inline void inline_static_3 (void);
  extern inline void inline_static_4 (void);
}

void
inline_3 (void)
{
}

void
inline_4 (void)
{
}
