/* PR c/82283 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

struct foo {
        const char *a1;
        const char * const *a2;
        void *a3;
        void *a4;
};

const char *aux[] = { "y", 0 };

struct foo a = {
  .a1 = "x",
  .a2 = (const char * const []){ "y", 0 },
}; /* { dg-bogus "missing initializer" } */

struct foo b = {
  .a2 = (const char * const []){ "y", 0 },
  .a1 = "x",
};
