// Build don't link: 
// GROUPS passed parsing
// parsing folder
// From: "James S. Vera" <vera@fanaraaken.stanford.edu>
// Date:     Thu, 01 Jul 1993 16:36:32 -0700
// Subject:  Mildly complicated type not understood, 2.4.5
// Message-ID: <9307012336.AA13841@fanaraaken.Stanford.EDU>

typedef  int (*cow[3])(...);

int main() {
  cow fs;
  int (*pig[3])(...); // line 5

}
