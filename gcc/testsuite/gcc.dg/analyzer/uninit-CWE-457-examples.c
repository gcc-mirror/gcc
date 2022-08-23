/* Examples adapted from https://cwe.mitre.org/data/definitions/457.html
   which states "Copyright © 2006–2022, The MITRE Corporation. CWE, CWSS, CWRAF, and the CWE logo are trademarks of The MITRE Corporation."
   and which has this on:
     https://cwe.mitre.org/about/termsofuse.html

   Terms of Use

   CWE™ is free to use by any organization or individual for any research, development, and/or commercial purposes, per these CWE Terms of Use. The MITRE Corporation ("MITRE") has copyrighted the CWE List, Top 25, CWSS, and CWRAF for the benefit of the community in order to ensure each remains a free and open standard, as well as to legally protect the ongoing use of it and any resulting content by government, vendors, and/or users. CWE is a trademark of MITRE. Please contact cwe@mitre.org if you require further clarification on this issue.

   LICENSE

   CWE Submissions: By submitting materials to The MITRE Corporation’s ("MITRE") Common Weakness Enumeration Program (CWE™), you hereby grant to MITRE a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable copyright license to use, reproduce, prepare derivative works of, publicly display, publicly perform, sublicense, and distribute your submitted materials and derivative works. Unless otherwise required by applicable law or agreed to in writing, it is understood that you are providing such materials on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limitation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE.

   CWE Usage: MITRE hereby grants you a non-exclusive, royalty-free license to use CWE for research, development, and commercial purposes. Any copy you make for such purposes is authorized on the condition that you reproduce MITRE’s copyright designation and this license in any such copy.

   DISCLAIMERS

   ALL DOCUMENTS AND THE INFORMATION CONTAINED IN THE CWE ARE PROVIDED ON AN "AS IS" BASIS AND THE CONTRIBUTOR, THE ORGANIZATION HE/SHE REPRESENTS OR IS SPONSORED BY (IF ANY), THE MITRE CORPORATION, ITS BOARD OF TRUSTEES, OFFICERS, AGENTS, AND EMPLOYEES, DISCLAIM ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE INFORMATION THEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.

   IN NO EVENT SHALL THE CONTRIBUTOR, THE ORGANIZATION HE/SHE REPRESENTS OR IS SPONSORED BY (IF ANY), THE MITRE CORPORATION, ITS BOARD OF TRUSTEES, OFFICERS, AGENTS, AND EMPLOYEES BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE INFORMATION OR THE USE OR OTHER DEALINGS IN THE CWE.  */

#include <stdio.h>

/* (example 1 from the page is written in PHP).  */

/* Example 2.  */

extern int repaint(int, int);

#define NEXT_SZ 42

void example_2_bad_code (int ctl, int i)
{
  int aN, bN; /* { dg-message "11: region created on stack here" } */
  switch (ctl) { /* { dg-message "following 'default:' branch\\.\\.\\." } */
  case -1:
    aN = 0;
    bN = 0;
    break;

  case 0:
    aN = i;
    bN = -i;
    break;

  case 1:
    aN = i + NEXT_SZ;
    bN = i - NEXT_SZ;
    break;

  default:
    aN = -1;
    aN = -1;
    break;
  }
  repaint(aN, bN); /* { dg-warning "use of uninitialized value 'bN'" } */
}

void example_2_fixed (int ctl, int i)
{
  int aN, bN;
  switch (ctl) {
  case -1:
    aN = 0;
    bN = 0;
    break;

  case 0:
    aN = i;
    bN = -i;
    break;

  case 1:
    aN = i + NEXT_SZ;
    bN = i - NEXT_SZ;
    break;

  default:
    aN = -1;
    bN = -1; /* (fixing bN/aN typo)  */
    break;
  }
  repaint(aN, bN);
}

/* Example 3.  */

void example_3_bad_code (int i, int err_val)
{
  char *test_string; /* { dg-message "9: region created on stack here" } */
  if (i != err_val)  /* { dg-message "following 'false' branch \\(when 'i == err_val'\\)\\.\\.\\." } */
  {
      test_string = "Hello World!";
  }
  printf("%s", test_string); /* { dg-warning "use of uninitialized value 'test_string'" } */
}

void example_3_fix_a (int i, int err_val)
{
  char *test_string = "Done at the beginning";
  if (i != err_val)
  {
      test_string = "Hello World!";
  }
  printf("%s", test_string);
}

void example_3_fix_b (int i, int err_val)
{
  char *test_string;
  if (i != err_val)
  {
      test_string = "Hello World!";
  }
  else {
    test_string = "Done on the other side!";
  }
  printf("%s", test_string);
}
