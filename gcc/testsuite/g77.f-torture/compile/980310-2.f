C unable to confirm this bug on egcs 1.0.1 for i586-pc-sco3.2v5.0.4 robertl
C
C Date: Sat, 23 Aug 1997 00:47:53 -0400 (EDT)
C From: David Bristow <dbristow@lynx.dac.neu.edu>
C To: egcs-bugs@cygnus.com
C Subject: g77 crashes compiling Dungeon
C Message-ID: <Pine.OSF.3.91.970823003521.11281A-100000@lynx.dac.neu.edu>
C
C The following small segment of Dungeon (the adventure that became the 
C commercial hit Zork) causes an internal error in f771.  The platform is 
C i586-pc-linux-gnulibc1, the compiler is egcs-ss-970821 (g77-GNU Fortran 
C 0.5.21-19970811)
C 
C --cut here--cut here--cut here--cut here--cut here--cut here--
C g77 --verbose -fugly -fvxt -c subr_.f
C g77 version 0.5.21-19970811
C  gcc --verbose -fugly -fvxt -xf77 subr_.f -xnone -lf2c -lm
C Reading specs from /usr/lib/gcc-lib/i586-pc-linux-gnulibc1/egcs-2.90.01/specs
C gcc version egcs-2.90.01 970821 (gcc2-970802 experimental)
C  /usr/lib/gcc-lib/i586-pc-linux-gnulibc1/egcs-2.90.01/f771 subr_.f -fset-g77-defaults -quiet -dumpbase subr_.f -version -fversion -fugly -fvxt -o /tmp/cca23974.s
C f771: warning: -fugly is overloaded with meanings and likely to be removed;
C f771: warning: use only the specific -fugly-* options you need
C GNU F77 version egcs-2.90.01 970821 (gcc2-970802 experimental) (i586-pc-linux-gnulibc1) compiled by GNU C version egcs-2.90.01 970821 (gcc2-970802 experimental).
C GNU Fortran Front End version 0.5.21-19970811
C f/com.c:941: failed assertion `TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (e))'
C gcc: Internal compiler error: program f771 got fatal signal 6
C --cut here--cut here--cut here--cut here--cut here--cut here--
C 
C Here's the FORTRAN code, it's basically a single subroutine from subr.f 
C in the Dungeon source, slightly altered (the original calls RAN(), which 
C doesn't exist in the g77 runtime)
C 
C RND - Return a random integer mod n
C
	INTEGER FUNCTION RND (N)
	IMPLICIT INTEGER (A-Z)
	REAL RAND
	COMMON /SEED/ RNSEED

	RND = RAND(RNSEED)*FLOAT(N)
	RETURN

	END
