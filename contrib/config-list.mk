# Run tests covering all config.gcc cases.
host_options='--with-mpc=/opt/cfarm/mpc' # gcc10
TEST=all-gcc
# Make sure you have a recent enough gcc (with ada support) in your path so
# that --enable-werror-always will work.
# To use, create a sibling directory to the gcc sources and cd into this.
# Use -j / -l make arguments and nice to assure a smooth resource-efficient
# load on the build machine, e.g. for 24 cores:
# svn co svn://gcc.gnu.org/svn/gcc/branches/foo-branch gcc
# mkdir multi-mk; cd multi-mk
# nohup nice make -j25 -l36 -f ../gcc/contrib/config-list.mk > make.out 2>&1 &
#
# v850e1-elf is rejected by config.sub
LIST = alpha-linux-gnu alpha-freebsd6 alpha-netbsd alpha-openbsd \
  alpha-dec-osf5.1OPT-enable-obsolete alpha64-dec-vms alpha-dec-vms \
  am33_2.0-linux \
  arm-wrs-vxworks arm-freebsd6 arm-netbsdelf arm-linux \
  arm-linux-androideabi arm-uclinux_eabi arm-ecos-elf arm-eabi \
  arm-symbianelf arm-rtems arm-elf arm-wince-pe avr-rtems avr-elf \
  bfin-elf bfin-uclinux bfin-linux-uclibc bfin-rtems bfin-openbsd \
  c6x-elf c6x-uclinux cr16-elf cris-elf cris-linux crisv32-elf crisv32-linux \
  epiphany-elf epiphany-elfOPT-with-stack-offset=16 fido-elf \
  fr30-elf frv-elf frv-linux h8300-elf h8300-rtems hppa-linux-gnu \
  hppa-linux-gnuOPT-enable-sjlj-exceptions=yes hppa64-linux-gnu \
  hppa2.0-hpux10.1 hppa64-hpux11.3 \
  hppa64-hpux11.0OPT-enable-sjlj-exceptions=yes hppa2.0-hpux11.9 \
  i686-pc-linux-gnu i686-apple-darwin i686-apple-darwin9 i686-apple-darwin10 \
  i486-freebsd4 i686-freebsd6 i686-kfreebsd-gnu \
  i686-netbsdelf9 i686-knetbsd-gnu i686-openbsd i686-openbsd3.0 \
  i686-elf i686-kopensolaris-gnu i686-symbolics-gnu i686-pc-msdosdjgpp \
  i686-lynxos i686-nto-qnx \
  i686-rtems i686-solaris2.10 i686-wrs-vxworks \
  i686-wrs-vxworksae \
  i686-cygwinOPT-enable-threads=yes i686-mingw32crt ia64-elf \
  ia64-freebsd6 ia64-linux ia64-hpux ia64-hp-vms iq2000-elf lm32-elf \
  lm32-rtems lm32-uclinux m32c-rtems m32c-elf m32r-elf m32rle-elf m32r-rtems \
  m32r-linux m32rle-linux m68k-elf m68k-netbsdelf \
  m68k-openbsd m68k-uclinux m68k-linux m68k-rtems \
  mcore-elf mep-elf microblaze-linux microblaze-elf \
  mips-sgi-irix6.5OPT-with-stabsOPT-enable-threads=posixOPT-enable-obsolete \
  mips-netbsd \
  mips64el-st-linux-gnu mips64octeon-linux mipsisa64r2-linux \
  mipsisa32r2-linux-gnu mips-openbsd mipsisa64r2-sde-elf mipsisa32-elfoabi \
  mipsisa64-elfoabi mipsisa64r2el-elf mipsisa64sr71k-elf mipsisa64sb1-elf \
  mipsel-elf mips64-elf mips64vr-elf mips64orion-elf mips-rtems \
  mips-wrs-vxworks mipstx39-elf mmix-knuth-mmixware mn10300-elf moxie-elf \
  moxie-uclinux moxie-rtems pdp11-aout picochip-elf powerpc-darwin8 \
  powerpc-darwin7 powerpc64-darwin powerpc-freebsd6 powerpc-netbsd \
  powerpc-eabispe powerpc-eabisimaltivec powerpc-eabisim ppc-elf \
  powerpc-eabialtivec powerpc-xilinx-eabi powerpc-eabi \
  powerpc-rtems4.11OPT-enable-threads=yes powerpc-linux_spe \
  powerpc-linux_paired powerpc64-linux_altivec \
  powerpc-wrs-vxworks powerpc-wrs-vxworksae powerpc-lynxos powerpcle-elf \
  powerpcle-eabisim powerpcle-eabi rs6000-ibm-aix4.3 rs6000-ibm-aix5.1.0 \
  rs6000-ibm-aix5.2.0 rs6000-ibm-aix5.3.0 rs6000-ibm-aix6.0 \
  rl78-elf rx-elf s390-linux-gnu s390x-linux-gnu s390x-ibm-tpf sh-elf \
  shle-linux sh-netbsdelf sh-superh-elf sh5el-netbsd sh64-netbsd sh64-linux \
  sh64-elfOPT-with-newlib sh-rtems sh-wrs-vxworks sparc-elf \
  sparc-leon-elf sparc-rtems sparc-linux-gnu \
  sparc-leon3-linux-gnuOPT-enable-target=all sparc-netbsdelf \
  sparc64-sun-solaris2.10OPT-with-gnu-ldOPT-with-gnu-asOPT-enable-threads=posix \
  sparc-wrs-vxworks sparc64-elf sparc64-rtems sparc64-linux sparc64-freebsd6 \
  sparc64-netbsd sparc64-openbsd spu-elf tilegx-linux-gnu tilepro-linux-gnu \
  v850e-elf v850-elf vax-linux-gnu \
  vax-netbsdelf vax-openbsd x86_64-apple-darwin \
  x86_64-pc-linux-gnuOPT-with-fpmath=avx \
  x86_64-elfOPT-with-fpmath=sse x86_64-freebsd6 x86_64-netbsd \
  x86_64-knetbsd-gnu x86_64-w64-mingw32 \
  x86_64-mingw32OPT-enable-sjlj-exceptions=yes xstormy16-elf xtensa-elf \
  xtensa-linux sparc-sun-solaris2.9 i686-solaris2.9 \
  i686-interix3OPT-enable-obsolete score-elfOPT-enable-obsolete

LOGFILES = $(patsubst %,log/%-make.out,$(LIST))
all: $(LOGFILES)
config: $(LIST)

.PHONY: make-log-dir all config

empty=

#Check for the presence of the MAINTAINERS file to make sure we are in a
#suitable current working directory.
make-log-dir: ../gcc/MAINTAINERS
	mkdir log

$(LIST): make-log-dir
	-mkdir $@
	(cd $@ && \
	../../gcc/configure \
	--target=$(subst SCRIPTS,`pwd`/../scripts/,$(subst OPT,$(empty) -,$@)) \
	--enable-werror-always ${host_options} --enable-languages=all,ada,go) \
	> log/$@-config.out 2>&1

$(LOGFILES) : log/%-make.out : %
	-$(MAKE) -C $< $(TEST) > $@ 2>&1 && rm -rf $<
