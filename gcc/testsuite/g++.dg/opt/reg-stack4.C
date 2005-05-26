// PR target/12900
// Origin: <snyder@fnal.gov>

// This used to fail on x86 because the reg-stack pass
// deleted a valid edge.

// { dg-do compile }
// { dg-options "-mtune=i586 -O2" { target { i?86-*-* && ilp32 } } }
// { dg-options "-mtune=i586 -O2" { target { x86_64-*-* && ilp32 } } }

struct array {
  double data;
  virtual ~array();
};

double glob;
double ext1(double);
int nmuons;

void track_match()
{
  array vecdca;
  if (glob < 10) return;
  double p = glob*5;
  double phi = vecdca.data;
  ext1 (vecdca.data-glob);
  ext1 (phi*2);
  if (1 < p)
    ++nmuons; 
}
