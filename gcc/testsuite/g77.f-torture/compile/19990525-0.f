* Mailing-List: contact egcs-bugs-help@egcs.cygnus.com; run by ezmlm
* Precedence: bulk
* Sender: owner-egcs-bugs@egcs.cygnus.com
* From: "Bjorn R. Bjornsson" <brb@halo.hi.is>
* Subject: g77 char expr. as arg to subroutine bug
* To: egcs-bugs@egcs.cygnus.com
* Date: Tue, 25 May 1999 14:45:56 +0000 (GMT)
* Content-Type: text/plain; charset=US-ASCII
* X-UIDL: 06000c94269ed6dfe826493e52a818b9
* 
* The following bug is in all snapshots starting
* from April 18.  I have only tested this on Alpha linux,
* and with FFECOM_FASTER_ARRAY_REFS set to 1.
* 
* Run the following through g77:
* 
	subroutine	a
	character*2	string1
	character*2	string2
	character*4	string3
	string1 = 's1'
	string2 = 's2'
c
c the next 2 lines are ok.
	string3 = (string1 // string2)
	call b(string1//string2)
c
c this line gives gcc/f/com.c:10660: failed assertion `hook'
	call b((string1//string2))
	end
* 
* the output from:
* 
* 	/usr/local/egcs-19990418/bin/g77 --verbose -c D.f
* 
* is:
* 
* on egcs-2.93.19 19990418 (gcc2 ss-980929 experimental) (from FSF-g77 version 0.5.24-19990418)
* Reading specs from /usr/local/egcs-19990418/lib/gcc-lib/alphaev56-unknown-linux-gnu/egcs-2.93.19/specs
* gcc version egcs-2.93.19 19990418 (gcc2 ss-980929 experimental)
*  /usr/local/egcs-19990418/lib/gcc-lib/alphaev56-unknown-linux-gnu/egcs-2.93.19/f771 D.f -quiet -dumpbase D.f -version -fversion -o /tmp/ccNpaaaa.s
* GNU F77 version egcs-2.93.19 19990418 (gcc2 ss-980929 experimental) (alphaev56-unknown-linux-gnu) compiled by GNU C version egcs-2.93.19 19990418 (gcc2 ss-980929 experimental).
* GNU Fortran Front End version 0.5.24-19990418
* ../../../egcs-19990418/gcc/f/com.c:10351: failed assertion `hook'
* g77: Internal compiler error: program f771 got fatal signal 6
* 
* Yours,
* 
* Bjorn R. Bjornsson
* brb@halo.hi.is
