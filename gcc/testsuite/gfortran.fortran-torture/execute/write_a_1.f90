! pr 15311
! output with 'A' edit descriptor
      program write_a_1
      character*25 s
! string = format
      write(s,'(A11)') "hello world"
      if (s.ne."hello world") call abort
! string < format
      write(s,'(A2)') "hello world"
      if (s.ne."he") call abort
! string > format
      write(s,'(A18)') "hello world"
      if (s.ne."       hello world") call abort
      end
