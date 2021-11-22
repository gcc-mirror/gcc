/* PR c/33925 - missing -Waddress with the address of an inline function
   { dg-do compile }
   { dg-options "-Wall" } */

struct A
{
  int mf ();
  int mf_def () { return 0; }

  static int smf ();
  static int smf_def () { return 0; }

  int mi;
  static int smi;
  static int smi_def;

  __attribute__ ((weak)) static int wsmi;
  __attribute__ ((weak)) static int wsmi_def;

  int mia[];
  static int smia[];
  static int smia_def[];

  __attribute__ ((weak)) static int wsmia[];
  __attribute__ ((weak)) static int wsmia_def[];

  void test_waddress (bool*);
};


/* __attribute__ ((weak)) static */ int A::smi_def = 0;
/* __attribute__ ((weak)) static */ int A::smia_def[] = { 0 };

/* __attribute__ ((weak)) static */ int A::wsmi_def = 0;
/* __attribute__ ((weak)) static */ int A::wsmia_def[] = { 0 };



void A::test_waddress (bool *p)
{
  if (mf)                               // { dg-error "cannot convert" }
    p++;
  if (mf_def)                           // { dg-error "cannot convert" }
    p++;

  if (smf)                              // { dg-warning "-Waddress" }
    p++;
  if (smf_def)                          // { dg-warning "-Waddress" }
    p++;

  if (&mi)                              // { dg-warning "-Waddress" }
    p++;
  if (&smi)                             // { dg-warning "-Waddress" }
    p++;
  if (&smi_def)                         // { dg-warning "-Waddress" }
    p++;

  if (&wsmi)
    p++;

  if (&wsmi_def)                        // { dg-warning "-Waddress" }
    p++;

  if (mia)                              // { dg-warning "-Waddress" }
    p++;
  if (smia)                             // { dg-warning "-Waddress" }
    p++;
  if (smia_def)                         // { dg-warning "-Waddress" }
    p++;

  if (wsmia)
    p++;

  if (wsmia_def)                        // { dg-warning "-Waddress" }
    p++;
}
