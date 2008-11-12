/* PR 35635 */
/* { dg-do compile } */
/* { dg-options "-Wconversion -Wsign-conversion" } */

struct unsigned_bit {
  unsigned int x:1;
} unsigned_bit;
struct signed_bit {
  int x:1;
} signed_bit;
int bar;
int bar2;

void func1()
{
  /* The result of boolean operators fits in unsiged int:1, thus do
     not warn.  */
  unsigned_bit.x = (bar != 0); /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar == 0); /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar <= 0); /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar >= 0); /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar < 0);  /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar > 0);  /* { dg-bogus "conversion" } */
  unsigned_bit.x = !bar;       /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar || bar2); /* { dg-bogus "conversion" } */
  unsigned_bit.x = (bar && bar2); /* { dg-bogus "conversion" } */

  /* Both branches of ? fit in the destination, thus do not warn.  */
  unsigned_bit.x = bar != 0 ? 1 : 0; /* { dg-bogus "conversion" } */
  unsigned_bit.x = bar != 0 ? 1.0 : 0.0;  /* { dg-bogus "conversion" } */

  /* At least one branch of ? does not fit in the destination, thus
     warn.  */
  unsigned_bit.x = bar != 0 ? 2 : 0; /* { dg-warning "conversion" } */
  unsigned_bit.x = bar != 0 ? 0 : -1; /* { dg-warning "conver" } */
}

void func2()
{
  signed char schar_x;

  /* Both branches of ? fit in the destination, thus do not warn.  */
  schar_x = bar != 0 ? 1 : 0; /* { dg-bogus "conversion" } */
  schar_x = bar != 0 ? 2.0 : 10; /* { dg-bogus "conversion" } */

  /* At least one branch of ? does not fit in the destination, thus
     warn.  */
  schar_x = bar != 0 ? 2.1 : 10; /* { dg-warning "conversion" } */
  schar_x = bar != 0 ? (signed char) 1024: -1024; /* { dg-warning "conversion" } */
}



void func3()
{
  unsigned char uchar_x;

  /* Both branches of ? fit in the destination, thus do not warn.  */
  uchar_x = bar != 0 ? 1 : 0;
  uchar_x = bar != 0 ? 2.0 : 10;

  /* At least one branch of ? does not fit in the destination, thus
     warn.  */
  uchar_x = bar != 0 ? 2.1 : 10; /* { dg-warning "conversion" } */
  uchar_x = bar != 0 
    ? (unsigned char) 1024 
    : -1; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
}

void func4()
{
  signed_bit.x = -1; /* { dg-bogus "conversion" } */
  signed_bit.x = bar != 0 ? -1.0 : 0.0;  /* { dg-bogus "conversion" } */
  signed_bit.x = bar != 0 ? -1 : 0; /* { dg-bogus "conversion" } */
  
  signed_bit.x = 1;          /* { dg-warning "conversion" } */
  signed_bit.x = (bar != 0); /* { dg-warning "conversion" } */
  signed_bit.x = (bar == 0); /* { dg-warning "conversion" } */
  signed_bit.x = (bar <= 0); /* { dg-warning "conversion" } */
  signed_bit.x = (bar >= 0); /* { dg-warning "conversion" } */
  signed_bit.x = (bar < 0);  /* { dg-warning "conversion" } */
  signed_bit.x = (bar > 0);  /* { dg-warning "conversion" } */
  signed_bit.x = !bar;       /* { dg-warning "conversion" } */
  signed_bit.x = (bar || bar2); /* { dg-warning "conversion" } */
  signed_bit.x = (bar && bar2); /* { dg-warning "conversion" } */
  signed_bit.x = bar != 0 ? 1 : 0; /* { dg-warning "conversion" } */
  signed_bit.x = bar != 0 ? 2 : 0; /* { dg-warning "conversion" } */
}

