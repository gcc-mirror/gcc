/* { dg-do compile } */
/* { dg-options -Wredundant-decls } */

/* Test to see if spurious warnings about redundant
   declarations are emiited because of the friend
   declaration.  */

class Foo
{
   friend void bar (Foo);
public:
};

extern void bar (Foo);
