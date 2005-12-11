c { dg-do compile }
* Resent-From: Craig Burley <burley@gnu.org>
* Resent-To: craig@jcb-sc.com
* X-Delivered: at request of burley on mescaline.gnu.org
* Date: Wed, 16 Dec 1998 18:31:24 +0100
* From: Dieter Stueken <stueken@conterra.de>
* Organization: con terra GmbH
* To: fortran@gnu.org
* Subject: possible bug
* Content-Type: text/plain; charset=iso-8859-1
* X-Mime-Autoconverted: from 8bit to quoted-printable by mescaline.gnu.org id KAA09085
* X-UIDL: 72293bf7f9fac8378ec7feca2bccbce2
* 
* Hi,
* 
* I'm about to compile a very old, very ugly Fortran program.
* For one part I got:
* 
* f77: Internal compiler error: program f771 got fatal signal 6
* 
* instead of any detailed error message. I was able to break down the
* problem to the following source fragment:
* 
* -------------------------------------------
        PROGRAM WAP

        integer*2  ios
        character*80  name

        name = 'blah'
        open(unit=8,status='unknown',file=name,form='formatted',
     F       iostat=ios) ! { dg-warning "integer kind in IOSTAT" }

      END
* -------------------------------------------
* 
* The problem seems to be caused by the "integer*2 ios" declaration.
* So far I solved it by simply using a plain integer instead.
* 
* I'm running gcc on a Linux system compiled/installed
* with no special options:
* 
* -> g77 -v
* g77 version 0.5.23
* Driving: g77 -v -c -xf77-version /dev/null -xnone
* Reading specs from /usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1/specs
* gcc version 2.8.1
*  /usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1/cpp -lang-c -v -undef
* -D__GNUC__=2 -D__GNUC_MINOR__=8 -D__ELF__ -D__unix__ -D__linux__
* -D__unix -D__linux -Asystem(posix) -D_LANGUAGE_FORTRAN -traditional
* -Di386 -Di686 -Asystem(unix) -Acpu(i386) -Amachine(i386) -D__i386__
* -D__i686__ -Asystem(unix) -Acpu(i386) -Amachine(i386) /dev/null
* /dev/null
* GNU CPP version 2.8.1 (i386 GNU/Linux with ELF)
* #include "..." search starts here:
* #include <...> search starts here:
*  /usr/local/include
*  /usr/i686-pc-linux-gnulibc1/include
*  /usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1/include
*  /usr/include
* End of search list.
*  /usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1/f771 -fnull-version
* -quiet -dumpbase g77-version.f -version -fversion -o /tmp/cca24911.s
* /dev/null
* GNU F77 version 2.8.1 (i686-pc-linux-gnulibc1) compiled by GNU C version
* 2.8.1.
* GNU Fortran Front End version 0.5.23
*  as -V -Qy -o /tmp/cca24911.o /tmp/cca24911.s
* GNU assembler version 2.8.1 (i486-linux), using BFD version 2.8.1
*  ld -m elf_i386 -dynamic-linker /lib/ld-linux.so.1 -o /tmp/cca24911
* /tmp/cca24911.o /usr/lib/crt1.o /usr/lib/crti.o
* /usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1/crtbegin.o
* -L/usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1 -L/usr -lg2c -lm -lgcc
* -lc -lgcc /usr/lib/gcc-lib/i686-pc-linux-gnulibc1/2.8.1/crtend.o
* /usr/lib/crtn.o
*  /tmp/cca24911
* __G77_LIBF77_VERSION__: 0.5.23
* @(#)LIBF77 VERSION 19970919
* __G77_LIBI77_VERSION__: 0.5.23
* @(#) LIBI77 VERSION pjw,dmg-mods 19980405
* __G77_LIBU77_VERSION__: 0.5.23
* @(#) LIBU77 VERSION 19970919
* 
* 
* Regards, Dieter.
* -- 
* Dieter Stüken, con terra GmbH, Münster
*     stueken@conterra.de         stueken@qgp.uni-muenster.de
*     http://www.conterra.de/     http://qgp.uni-muenster.de/~stueken
*     (0)251-980-2027             (0)251-83-334974
