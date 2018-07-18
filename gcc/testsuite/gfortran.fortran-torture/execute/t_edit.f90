!pr 14897  T edit descriptor broken
      implicit none
      character*80 line
      WRITE(line,'(T5,A,T10,A,T15,A)')'AA','BB','CC'
      if (line.ne.'    AA   BB   CC    ') STOP 1
      WRITE(line,'(5HAAAAA,TL4,4HABCD)')
      if (line.ne.'AABCD') STOP 2
      END



