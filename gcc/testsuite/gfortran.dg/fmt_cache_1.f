! { dg-do run { target fd_truncate } }
! pr40662 segfaults when specific format is invoked twice.
! pr40330  incorrect io.
! test case derived from pr40662, <jvdelisle@gcc.gnu.org>
      program astap
      implicit none
      character(34) :: teststring
      real(4) :: arlxca = 0.0
      open(10)
      write(10,40) arlxca
      write(10,40) arlxca
40    format(t4,"arlxca = ",1pg13.6,t27,"arlxcc = ",g13.6,t53,
     .            "atmpca = ",g13.6,t79,"atmpcc = ",g13.6,t105,
     .            "backup = ",g13.6,/,
     .         t4,"csgfac = ",g13.6,t27,"csgmax = ",g13.6,t53,
     .            "csgmin = ",g13.6,t79,"drlxca = ",g13.6,t105,
     .            "drlxcc = ",g13.6,/,
     .         t4,"dtimeh = ",g13.6,t27,"dtimei = ",g13.6,t53,
     .            "dtimel = ",g13.6,t79,"dtimeu = ",g13.6,t105,
     .            "dtmpca = ",g13.6,/,
     .         t4,"dtmpcc = ",g13.6,t27,"ebalna = ",g13.6,t53,
     .            "ebalnc = ",g13.6,t79,"ebalsa = ",g13.6,t105,
     .            "ebalsc = ",g13.6)
      rewind 10
      teststring = ""
      read(10,'(a)') teststring
      if (teststring.ne."   arlxca =   0.00000     arlxcc =")call abort
      teststring = ""
      read(10,'(a)') teststring
      if (teststring.ne."   arlxca =   0.00000     arlxcc =")call abort
      close(10, status='delete')
      end program astap


