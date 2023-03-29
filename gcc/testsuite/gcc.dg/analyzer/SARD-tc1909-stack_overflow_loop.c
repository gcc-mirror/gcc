/* Adapted from
     https://samate.nist.gov/SARD/downloads/test-suites/2013-02-07-basic-cwe-effectiveness-cwe-121-stack-based-buffer-overflow-for-c.zip
   Part of https://samate.nist.gov/SARD/test-suites/81:
   See:
     Black, P. , Koo, H. and Irish, T. (2013), A Basic CWE-121 Buffer Overflow Effectiveness Test Suite, Proc. 6th Latin-American Symposium on Dependable Computing, Rio de Janeiro, -1, [online], https://tsapps.nist.gov/publication/get_pdf.cfm?pub_id=913117 (Accessed January 17, 2023)
*/

/* This software was developed at the National Institute of Standards and
 * Technology by employees of the Federal Government in the course of their
 * official duties. Pursuant to title 17 Section 105 of the United States
 * Code this software is not subject to copyright protection and is in the
 * public domain. NIST assumes no responsibility whatsoever for its use by
 * other parties, and makes no guarantees, expressed or implied, about its
 * quality, reliability, or any other characteristic.

 * We would appreciate acknowledgement if the software is used.
 * The SAMATE project website is: http://samate.nist.gov
*/

#include <stdlib.h>

int main(int argc, char *argv[])
{
	char bStr[10];
	for (unsigned i=1;i<=10;++i) {
		bStr[i] = (char)i + 'a'; /* { dg-warning "stack-based buffer overflow" "PR analyzer/108432" { xfail *-*-* } } */
	}
	return 0;
}
