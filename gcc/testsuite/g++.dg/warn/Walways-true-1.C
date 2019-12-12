// Test -Waddress for testing an address against NULL.
// Origin: Ian Lance Taylor <iant@google.com>

// { dg-do compile }
// { dg-options "-Waddress" }

extern int foo (int);

int i;

void
bar (int a)
{
 lab:
  if (foo)	// { dg-warning "7:the address of .int foo\\(int\\). will never be NULL" "correct warning" }
    foo (0);
  if (foo (1))
    ;
  if (&i)	// { dg-warning "7:the address of .i. will never be NULL" "correct warning" }
    foo (2);
  if (i)
    foo (3);
  if (&a)	// { dg-warning "7:the address of .a. will never be NULL" "correct warning" }
    foo (4);
  if (a)
    foo (5);
  if (&&lab)	// { dg-warning "7:the address of .lab. will never be NULL" "correct warning" }
    foo (6);
  if (foo == 0)	// { dg-warning "11:the address of .int foo\\(int\\). will never be NULL" "correct warning" }
    foo (7);
  if (foo (1) == 0)
    foo (8);
  if (&i == 0)	// { dg-warning "10:the address of .i. will never be NULL" "correct warning" }
    foo (9);
  if (i == 0)
    foo (10);
  if (&a == 0)	// { dg-warning "10:the address of .a. will never be NULL" "correct warning" }
    foo (11);
  if (a == 0)
    foo (12);
  if (&&lab == 0) // { dg-warning "13:the address of .lab. will never be NULL" "correct warning" }
    foo (13);
  if (0 == foo)	// { dg-warning "9:the address of .int foo\\(int\\). will never be NULL" "correct warning" }
    foo (14);
  if (0 == foo (1))
    foo (15);
  if (0 == &i)	// { dg-warning "9:the address of .i. will never be NULL" "correct warning" }
    foo (16);
  if (0 == i)
    foo (17);
  if (0 == &a)	// { dg-warning "9:the address of .a. will never be NULL" "correct warning" }
    foo (18);
  if (0 == a)
    foo (19);
  if (0 == &&lab) // { dg-warning "9:the address of .lab. will never be NULL" "correct warning" }
    foo (20);
}
