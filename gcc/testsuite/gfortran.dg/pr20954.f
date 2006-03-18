      ! { dg-do run }
      ! { dg-options "-fdefault-integer-8" }
      ! Program to test character length type
      Program pr20954
      character*16 string (5)
      character*5 filename
      character*80 line
      filename = 'input'
      open (2,file=filename)
      write (line, '(5a16)') (string(i),i=1,5)
      close (2, status='delete')
      end
