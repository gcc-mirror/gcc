*     Preliminary tests for a few things in the i/o library.
*     Thrown together by Dave Love not from specific bug reports --
*     other ideas welcome.

      character *(*) fmt
      parameter (fmt='(1x,i3,f5.1)')
*     Scratch file makes sure we can use one and avoids dealing with
*     explicit i/o in the testsuite.
      open(90, status='scratch') ! try a biggish unit number
      write(90, '()')           ! extra record for interest
*     Formatted i/o can go wild (endless loop AFAIR) if we're wrongly
*     assuming an ANSI sprintf.
      write(90, fmt) 123, 123.0
      backspace 90              ! backspace problems reported on DOSish systems
      read(90, fmt) i, r
      endfile 90
      if (i/=123 .or. nint(r)/=123) call abort
      rewind 90                 ! make sure we can rewind too
      read(90, '()')
      read(90, fmt) i, r
      if (i/=123 .or. nint(r)/=123) call abort
      close(90)
*     Make sure we can do unformatted i/o OK.  This might be
*     problematic on DOS-like systems if we've done an fopen in text
*     mode, not binary.     
      open(90, status='scratch', access='direct', form='unformatted',
     +     recl=8)
      write(90, rec=1) 123, 123.0
      read(90, rec=1) i, r
      if (i/=123 .or. nint(r)/=123) call abort
      close(90)
      open(90, status='scratch', form='unformatted')
      write(90) 123, 123.0
      backspace 90
      read(90) i, r
      if (i/=123 .or. nint(r)/=123) call abort
      close(90)
*     Fails at 1998-09-01 on spurious recursive i/o check (fixed by
*     1998-09-06 libI77 change):
      open(90, status='scratch', form='formatted', recl=16,
     +     access='direct')
      write(90, '(i8,f8.1)',rec=1) 123, 123.0
      read(90, '(i8,f8.1)', rec=1) i, r
      if (i/=123 .or. nint(r)/=123) call abort
      close(90)
      end
