/* Test to verify the handling of attribute access (none).
   { dg-do compile }
   { dg-options "-O -Wall -ftrack-macro-expansion=0" } */

int __attribute__ ((access (none, 1)))
fnone_pv1 (void*);

void nowarn_fnone_pv1 (void)
{
  int x;
  fnone_pv1 (&x);
}


int __attribute__ ((access (none, 1)))
fnone_pcv1 (const void*);

void nowarn_fnone_pcv1 (void)
{
  char a[2];
  fnone_pcv1 (a);
}


int __attribute__ ((access (none, 1, 2)))
fnone_pcv1_2 (const void*, int);  // { dg-message "in a call to function 'fnone_pcv1_2' declared with attribute 'none \\\(1, 2\\\)'" }

void nowarn_fnone_pcv1_2 (void)
{
  char a[2];
  fnone_pcv1_2 (a, 2);
}

void warn_fnone_pcv1_2 (void)
{
  char a[3];
  fnone_pcv1_2 (a, 4);        // { dg-warning "expecting 4 bytes in a region of size 3" }
}
