/* PR c/50584 - No warning for passing small array to C99 static array
   declarator
   Verify warnings for redeclarations of functions with pointer parameters
   to arrays with variable bounds involving typedefs.
   { dg-do compile }
   { dg-options "-Wall -Wvla-parameter" } */

extern int m, n;

typedef int IA3[3];

/* Verify the warning points to the declaration with more unspecified
   bounds, guiding the user to specify them rather than making them all
   unspecified.  */

void* f_pIA3an (IA3 *x[n]);              // { dg-message "previously declared as 'int \\\(\\\*\\\[n]\\\)\\\[3]' with 0 unspecified variable bounds" "note" }
void* f_pIA3an (IA3 *x[n]);
void* f_pIA3an (IA3 *x[*]);              // { dg-warning "argument 1 of type 'int \\\(\\\*\\\[\\\*]\\\)\\\[3]' .aka '\[^\n\r\}\]+'. declared with 1 unspecified variable bound" }
void* f_pIA3an (IA3 *x[n]) { return x; }


void nowarn_local_fndecl (void)
{
  typedef int IAm[m];

  void* f_IAm (IAm);
  void* f_IAm (int[m]);
  void* f_IAm (IAm);

  void* f_iam (int[m]);
  void* f_iam (IAm);
  void* f_iam (int[m]);

  typedef int     IA3[3];
  typedef IA3     IAn_3[n];
  typedef IAn_3   IA2_n_3[2];
  typedef IA2_n_3 IAm_2_n_3[m];

  void f_IAm_2_n_3 (IAm_2_n_3);
  void f_IAm_2_n_3 (IA2_n_3[m]);
  void f_IAm_2_n_3 (IAn_3[m][2]);
  void f_IAm_2_n_3 (IA3[m][2][n]);
  void f_IAm_2_n_3 (int[m][2][n][3]);

  void f_iam_2_n_3 (int[m][2][n][3]);
  void f_iam_2_n_3 (IA3[m][2][n]);
  void f_iam_2_n_3 (IAn_3[m][2]);
  void f_iam_2_n_3 (IAm_2_n_3);

  void f_IAx_m_2_n_3 (IAm_2_n_3[*]);
  void f_IAx_m_2_n_3 (IA2_n_3[*][m]);
  void f_IAx_m_2_n_3 (IAn_3[*][m][2]);
  void f_IAx_m_2_n_3 (IA3[*][m][2][n]);
  void f_IAx_m_2_n_3 (int[*][m][2][n][3]);

  void f_IA__m_2_n_3 (IAm_2_n_3[]);
  void f_IA__m_2_n_3 (IA2_n_3[][m]);
  void f_IA__m_2_n_3 (IAn_3[][m][2]);
  void f_IA__m_2_n_3 (IA3[][m][2][n]);
  void f_IA__m_2_n_3 (int[][m][2][n][3]);
}


void warn_local_fndecl (void)
{
  typedef int IAm[m];
  typedef int IAn[n];

  void* g_IAm (IAm);                    // { dg-message "previously declared as 'int\\\[m]' with bound 'm'" }
  void* g_IAm (int[n]);                 // { dg-warning "argument 1 of type 'int\\\[n]' declared with mismatched bound 'n'" }
  void* g_IAm (IAm);

  void* g_iam (int[m]);                 // { dg-message "previously declared as 'int\\\[m]' with bound 'm'" }
  void* g_iam (IAn);                    // { dg-warning "argument 1 of type 'int\\\[n]' declared with mismatched bound 'n'" }
  void* g_iam (int[m]);


  typedef int     IA3[3];
  typedef IA3     IAn_3[n];
  typedef IAn_3   IA2_n_3[2];
  typedef IA2_n_3 IAm_2_n_3[m];

  typedef IA3     IAm_3[m];
  typedef IAm_3   IA2_m_3[2];
  typedef IA2_m_3 IAm_2_m_3[m];

  void* g_IAm_2_n_3 (IAm_2_n_3);
  void* g_IAm_2_n_3 (int[m][2][m][3]);  // { dg-warning "argument 1 of type 'int\\\[m]\\\[2]\\\[m]\\\[3]' declared with mismatched bound 'm'" }
  void* g_IAm_2_n_3 (IAm_2_n_3);

  void* g_iam_2_n_2 (int[m][2][n][3]);
  void* g_iam_2_n_2 (IAm_2_m_3);        // { dg-warning "argument 1 of type 'int\\\[m]\\\[2]\\\[m]\\\[3]' declared with mismatched bound 'm'" }
  void* g_iam_2_n_2 (int[m][2][n][3]);
}
