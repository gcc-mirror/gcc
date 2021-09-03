/*
  { dg-do compile }
  { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

int memcmp (const void*, const void*, size_t);
int strncmp (const char*, const char*, size_t);
char* stpncpy (char*, const char*, size_t);
char* strncpy (char*, const char*, size_t);

extern char a4[4], b5[5];

struct A { char a4[4]; };

extern volatile int i;
extern void* volatile ptr;

void test_stpncpy (struct A *p)
{
  ptr = stpncpy (a4, b5, 4);
  ptr = stpncpy (a4, b5, 5);      // { dg-warning "writing 5 bytes" }

  ptr = stpncpy (p->a4, b5, 4);
  ptr = stpncpy (p->a4, b5, 5);   // { dg-warning "writing 5 bytes" }
}

void test_strncpy (struct A *p)
{
  ptr = strncpy (a4, b5, 4);
  ptr = strncpy (a4, b5, 5);      // { dg-warning "writing 5 bytes" }

  ptr = strncpy (p->a4, b5, 4);
  ptr = strncpy (p->a4, b5, 5);   // { dg-warning "writing 5 bytes" }
}
