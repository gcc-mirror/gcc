/* PR target/9365
 * Origin: marcus@mc.pp.se
 * Testcase tweaked by dank@kegel.com
 * gcc 3.4 coverage by joern.rennecke@superh.com
 * [3.3 regression] [SH] segfault in gen_far_branch (config/sh/sh.c)
 * ice-on-valid-code
 * Not marked as xfail since it's a regression
*/
/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */


void foo(int n, int *p)
{
	switch(n) {
	case 100: case 110: case 120: case 130: case 140: case 150: case 160:
	case 200: case 210: case 220: case 230: case 240: case 250: case 260:
	case 300: case 310: case 320: case 330: case 340: case 350: case 360:
	case 400: case 410: case 420: case 430: case 440: case 450: case 460:
	case 500: case 510: case 520: case 530: case 540: case 550: case 560:
	case 600: case 610: case 620: case 630: case 640: case 650: case 660:
	case 700: case 710: case 720: case 730: case 740: case 750: case 760:
	case 800: case 810: case 820: case 830: case 840: case 850: case 860:
	case 900: case 910: case 920: case 930: case 940: case 950: case 960:
		break;
	default:
		*p = n;
		break;
	}
}

int main(int argc, char **argv)
{
	int p;

	(void) argv;

	foo(argc, &p);

	return p;
}
