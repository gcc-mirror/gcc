/* 78696 - -fprintf-return-value misoptimizes %.Ng where N is greater than 10
   Test to verify the correctness of ranges of output computed for floating
   point directives.
   { dg-do compile }
   { dg-require-effective-target double64plus }
   { dg-skip-if "not IEEE float layout" { "pdp11-*-*" } }
   { dg-options "-O2 -Wformat -Wformat-overflow -ftrack-macro-expansion=0" } */

typedef __builtin_va_list va_list;

char dst[1];

extern void sink (int, void*);

/* Macro to test either width or precision specified by the asterisk
   (but not both).  */
#define T1(fmt, a)    sink (__builtin_sprintf (dst + 1, fmt, a, x), dst)

/* Macro to test both width and precision specified by the asterisk.  */
#define T2(fmt, w, p) sink (__builtin_sprintf (dst + 1, fmt, w, p, x), dst)

/* Macro to test vsprintf with both width and precision specified by
   the asterisk.  */
#define T(fmt) sink (__builtin_vsprintf (dst + 1, fmt, va), dst)

/* Exercise %a.  */
void test_a (int w, int p, double x)
{
  T1 ("%.*a", 0);     /* { dg-warning "between 3 and 10 bytes" } */
  T1 ("%.*a", 1);     /* { dg-warning "between 3 and 12 bytes" } */
  T1 ("%.*a", 2);     /* { dg-warning "between 3 and 13 bytes" } */
  T1 ("%.*a", 99);    /* { dg-warning "between 3 and 110 bytes" } */
  T1 ("%.*a", 199);   /* { dg-warning "between 3 and 210 bytes" } */
  T1 ("%.*a", 1099);  /* { dg-warning "between 3 and 1110 bytes" } */

  T1 ("%*.a", 0);     /* { dg-warning "between 3 and 10 bytes" } */
  T1 ("%*.a", 1);     /* { dg-warning "between 3 and 10 bytes" } */
  T1 ("%*.a", 3);     /* { dg-warning "between 3 and 10 bytes" } */
  T1 ("%*.a", 6);     /* { dg-warning "between 6 and 10 bytes" } */
  T1 ("%*.a", 7);     /* { dg-warning "between 7 and 10 bytes" } */

  T1 ("%*.a", w);     /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.0a", w);    /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.1a", w);    /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.2a", w);    /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */

  T1 ("%.*a",  p);    /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
  T1 ("%1.*a", p);    /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
  T1 ("%2.*a", p);    /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
  T1 ("%3.*a", p);    /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */

  T2 ("%*.*a", w, p); /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
}

/* Exercise %e.  */
void test_e (int w, int p, double x)
{
  T1 ("%.*e", 0);     /* { dg-warning "between 3 and 7 bytes" } */
  T1 ("%.*e", 1);     /* { dg-warning "between 3 and 9 bytes" } */
  T1 ("%.*e", 2);     /* { dg-warning "between 3 and 10 bytes" } */
  T1 ("%.*e", 99);    /* { dg-warning "between 3 and 107 bytes" } */
  T1 ("%.*e", 199);   /* { dg-warning "between 3 and 207 bytes" } */
  T1 ("%.*e", 1099);  /* { dg-warning "between 3 and 1107 bytes" } */

  T1 ("%*.e", 0);     /* { dg-warning "between 3 and 7 bytes" } */
  T1 ("%*.e", 1);     /* { dg-warning "between 3 and 7 bytes" } */
  T1 ("%*.e", 1);     /* { dg-warning "between 3 and 7 bytes" } */
  T1 ("%*.e", 3);     /* { dg-warning "between 3 and 7 bytes" } */
  T1 ("%*.e", 6);     /* { dg-warning "between 6 and 7 bytes" } */
  T1 ("%*.e", 7);     /* { dg-warning "writing 7 bytes" } */

  T1 ("%*.e", w);     /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.0e", w);    /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.1e", w);    /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.2e", w);    /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */

  T1 ("%.*e",  p);    /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
  T1 ("%1.*e", p);    /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
  T1 ("%2.*e", p);    /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
  T1 ("%3.*e", p);    /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */

  T2 ("%*.*e", w, p); /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
}

/* Exercise %f.  */
void test_f (int w, int p, double x)
{
  T1 ("%.*f", 0);           /* { dg-warning "between 1 and 310 bytes" } */
  T1 ("%.*f", 1);           /* { dg-warning "between 3 and 312 bytes" } */
  T1 ("%.*f", 2);           /* { dg-warning "between 3 and 313 bytes" } */
  T1 ("%.*f", 99);          /* { dg-warning "between 3 and 410 bytes" } */
  T1 ("%.*f", 199);         /* { dg-warning "between 3 and 510 bytes" } */
  T1 ("%.*f", 1099);        /* { dg-warning "between 3 and 1410 bytes" } */

  T2 ("%*.*f", 0, 0);       /* { dg-warning "between 1 and 310 bytes" } */
  T2 ("%*.*f", 1, 0);       /* { dg-warning "between 1 and 310 bytes" } */
  T2 ("%*.*f", 2, 0);       /* { dg-warning "between 2 and 310 bytes" } */
  T2 ("%*.*f", 3, 0);       /* { dg-warning "between 3 and 310 bytes" } */
  T2 ("%*.*f", 310, 0);     /* { dg-warning "writing 310 bytes" } */
  T2 ("%*.*f", 311, 0);     /* { dg-warning "writing 311 bytes" } */
  T2 ("%*.*f", 312, 312);   /* { dg-warning "between 312 and 623 bytes" } */
  T2 ("%*.*f", 312, 313);   /* { dg-warning "between 312 and 624 bytes" } */

  T1 ("%*.f", w);           /* { dg-warning "writing between 1 and (2147483648|32768) bytes" } */
  T1 ("%*.0f", w);          /* { dg-warning "writing between 1 and (2147483648|32768) bytes" } */
  T1 ("%*.1f", w);          /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T1 ("%*.2f", w);          /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */

  T1 ("%.*f",  p);          /* { dg-warning "writing between 1 and (2147483958|33078) bytes" } */
  T1 ("%1.*f", p);          /* { dg-warning "writing between 1 and (2147483958|33078) bytes" } */
  T1 ("%2.*f", p);          /* { dg-warning "writing between 2 and (2147483958|33078) bytes" } */
  T1 ("%3.*f", p);          /* { dg-warning "writing between 3 and (2147483958|33078) bytes" } */

  T2 ("%*.*f", w, p);       /* { dg-warning "writing between 1 and (2147483958|33078) bytes" } */
}

/* Exercise %g.  The expected output is the lesser of %e and %f.  */
void test_g (double x)
{
  T1 ("%.*g", 0);           /* { dg-warning "between 1 and 7 bytes" } */
  T1 ("%.*g", 1);           /* { dg-warning "between 1 and 7 bytes" } */
  T1 ("%.*g", 2);           /* { dg-warning "between 1 and 9 bytes" } */
  T1 ("%.*g", 99);          /* { dg-warning "between 1 and 106 bytes" } */
  T1 ("%.*g", 199);         /* { dg-warning "between 1 and 206 bytes" } */
  T1 ("%.*g", 1099);        /* { dg-warning "between 1 and 310 bytes" } */

  T2 ("%*.*g", 0, 0);       /* { dg-warning "between 1 and 7 bytes" } */
  T2 ("%*.*g", 1, 0);       /* { dg-warning "between 1 and 7 bytes" } */
  T2 ("%*.*g", 2, 0);       /* { dg-warning "between 2 and 7 bytes" } */
  T2 ("%*.*g", 3, 0);       /* { dg-warning "between 3 and 7 bytes" } */
  T2 ("%*.*g", 7, 0);       /* { dg-warning "writing 7 bytes" } */
  T2 ("%*.*g", 310, 0);     /* { dg-warning "writing 310 bytes" } */
  T2 ("%*.*g", 311, 0);     /* { dg-warning "writing 311 bytes" } */
  T2 ("%*.*g", 312, 312);   /* { dg-warning "writing 312 bytes" } */
  T2 ("%*.*g", 312, 313);   /* { dg-warning "writing 312 bytes" } */
  T2 ("%*.*g", 333, 999);   /* { dg-warning "writing 333 bytes" } */
}

/* Exercise %a.  */
void test_a_va (va_list va)
{
  T ("%.0a");       /* { dg-warning "between 3 and 10 bytes" } */
  T ("%.1a");       /* { dg-warning "between 3 and 12 bytes" } */
  T ("%.2a");       /* { dg-warning "between 3 and 13 bytes" } */
  T ("%.99a");      /* { dg-warning "between 3 and 110 bytes" } */
  T ("%.199a");     /* { dg-warning "between 3 and 210 bytes" } */
  T ("%.1099a");    /* { dg-warning "between 3 and 1110 bytes" } */

  T ("%0.a");       /* { dg-warning "between 3 and 10 bytes" } */
  T ("%1.a");       /* { dg-warning "between 3 and 10 bytes" } */
  T ("%3.a");       /* { dg-warning "between 3 and 10 bytes" } */
  T ("%6.a");       /* { dg-warning "between 6 and 10 bytes" } */
  T ("%7.a");       /* { dg-warning "between 7 and 10 bytes" } */

  T ("%*.a");       /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T ("%*.0a");      /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T ("%*.1a");      /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */
  T ("%*.2a");      /* { dg-warning "writing between 3 and (2147483648|32768) bytes" } */

  T ("%.*a");       /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
  T ("%1.*a");      /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
  T ("%2.*a");      /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
  T ("%6.*a");      /* { dg-warning "writing between 6 and (2147483658|32778) bytes" } */
  T ("%9.*a");      /* { dg-warning "writing between 9 and (2147483658|32778) bytes" } */

  T ("%*.*a");      /* { dg-warning "writing between 3 and (2147483658|32778) bytes" } */
}

/* Exercise %e.  */
void test_e_va (va_list va)
{
  T ("%e");         /* { dg-warning "between 3 and 14 bytes" } */
  T ("%+e");        /* { dg-warning "between 4 and 14 bytes" } */
  T ("% e");        /* { dg-warning "between 4 and 14 bytes" } */
  T ("%#e");        /* { dg-warning "between 3 and 14 bytes" } */
  T ("%#+e");       /* { dg-warning "between 4 and 14 bytes" } */
  T ("%# e");       /* { dg-warning "between 4 and 14 bytes" } */

  T ("%.e");        /* { dg-warning "between 3 and 7 bytes" } */
  T ("%.0e");       /* { dg-warning "between 3 and 7 bytes" } */
  T ("%.1e");       /* { dg-warning "between 3 and 9 bytes" } */
  T ("%.2e");       /* { dg-warning "between 3 and 10 bytes" } */
  T ("%.99e");      /* { dg-warning "between 3 and 107 bytes" } */
  T ("%.199e");     /* { dg-warning "between 3 and 207 bytes" } */
  T ("%.1099e");    /* { dg-warning "between 3 and 1107 bytes" } */

  T ("%0.e");       /* { dg-warning "between 3 and 7 bytes" } */
  T ("%1.e");       /* { dg-warning "between 3 and 7 bytes" } */
  T ("%1.e");       /* { dg-warning "between 3 and 7 bytes" } */
  T ("%3.e");       /* { dg-warning "between 3 and 7 bytes" } */
  T ("%6.e");       /* { dg-warning "between 6 and 7 bytes" } */
  T ("%7.e");       /* { dg-warning "writing 7 bytes" } */

  T ("%.*e");       /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
  T ("%1.*e");      /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
  T ("%6.*e");      /* { dg-warning "writing between 6 and (2147483655|32775) bytes" } */
  T ("%9.*e");      /* { dg-warning "writing between 9 and (2147483655|32775) bytes" } */

  T ("%*.*e");      /* { dg-warning "writing between 3 and (2147483655|32775) bytes" } */
}

/* Exercise %f.  */
void test_f_va (va_list va)
{
  T ("%f");         /* { dg-warning "between 3 and 317 bytes" } */
  T ("%+f");        /* { dg-warning "between 4 and 317 bytes" } */
  T ("% f");        /* { dg-warning "between 4 and 317 bytes" } */
  T ("%#f");        /* { dg-warning "between 3 and 317 bytes" } */
  T ("%+f");        /* { dg-warning "between 4 and 317 bytes" } */
  T ("% f");        /* { dg-warning "between 4 and 317 bytes" } */
  T ("%#+f");       /* { dg-warning "between 4 and 317 bytes" } */
  T ("%# f");       /* { dg-warning "between 4 and 317 bytes" } */

  T ("%.f");        /* { dg-warning "between 1 and 310 bytes" } */
  T ("%.0f");       /* { dg-warning "between 1 and 310 bytes" } */
  T ("%.1f");       /* { dg-warning "between 3 and 312 bytes" } */
  T ("%.2f");       /* { dg-warning "between 3 and 313 bytes" } */
  T ("%.99f");      /* { dg-warning "between 3 and 410 bytes" } */
  T ("%.199f");     /* { dg-warning "between 3 and 510 bytes" } */
  T ("%.1099f");    /* { dg-warning "between 3 and 1410 bytes" } */

  T ("%0.0f");      /* { dg-warning "between 1 and 310 bytes" } */
  T ("%1.0f");      /* { dg-warning "between 1 and 310 bytes" } */
  T ("%2.0f");      /* { dg-warning "between 2 and 310 bytes" } */
  T ("%3.0f");      /* { dg-warning "between 3 and 310 bytes" } */
  T ("%310.0f");    /* { dg-warning "writing 310 bytes" } */
  T ("%311.0f");    /* { dg-warning "writing 311 bytes" } */
  T ("%312.312f");  /* { dg-warning "between 312 and 623 bytes" } */
  T ("%312.313f");  /* { dg-warning "between 312 and 624 bytes" } */

  T ("%.*f");       /* { dg-warning "writing between 1 and (2147483958|33078) bytes" } */
  T ("%1.*f");      /* { dg-warning "writing between 1 and (2147483958|33078) bytes" } */
  T ("%3.*f");      /* { dg-warning "writing between 3 and (2147483958|33078) bytes" } */

  T ("%*.*f");      /* { dg-warning "writing between 1 and (2147483958|33078) bytes" } */
}

/* Exercise %g.  The expected output is the lesser of %e and %f.  */
void test_g_va (va_list va)
{
  T ("%g");         /* { dg-warning "between 1 and 13 bytes" } */
  T ("%+g");        /* { dg-warning "between 2 and 13 bytes" } */
  T ("% g");        /* { dg-warning "between 2 and 13 bytes" } */

  /* The pound flag means the radix character is always present.  */
  T ("%#g");        /* { dg-warning "between 2 and 13 bytes" } */
  T ("%#+g");       /* { dg-warning "between 3 and 13 bytes" } */
  T ("%# g");       /* { dg-warning "between 3 and 13 bytes" } */

  T ("%.g");        /* { dg-warning "between 1 and 7 bytes" } */
  T ("%.0g");       /* { dg-warning "between 1 and 7 bytes" } */
  T ("%.1g");       /* { dg-warning "between 1 and 7 bytes" } */
  T ("%.2g");       /* { dg-warning "between 1 and 9 bytes" } */
  T ("%.99g");      /* { dg-warning "between 1 and 106 bytes" } */
  T ("%.199g");     /* { dg-warning "between 1 and 206 bytes" } */
  T ("%.1099g");    /* { dg-warning "between 1 and 310 bytes" } */

  T ("%0.0g");      /* { dg-warning "between 1 and 7 bytes" } */
  T ("%1.0g");      /* { dg-warning "between 1 and 7 bytes" } */
  T ("%2.0g");      /* { dg-warning "between 2 and 7 bytes" } */
  T ("%3.0g");      /* { dg-warning "between 3 and 7 bytes" } */
  T ("%7.0g");      /* { dg-warning "writing 7 bytes" } */
  T ("%310.0g");    /* { dg-warning "writing 310 bytes" } */
  T ("%311.0g");    /* { dg-warning "writing 311 bytes" } */
  T ("%312.312g");  /* { dg-warning "writing 312 bytes" } */
  T ("%312.313g");  /* { dg-warning "writing 312 bytes" } */
  T ("%333.999g");  /* { dg-warning "writing 333 bytes" } */

  T ("%.*g");       /* { dg-warning "writing between 1 and 310 bytes" } */
  T ("%1.*g");      /* { dg-warning "writing between 1 and 310 bytes" } */
  T ("%4.*g");      /* { dg-warning "writing between 4 and 310 bytes" } */

  T ("%*.*g");      /* { dg-warning "writing between 1 and (2147483648|32768) bytes" } */
}
