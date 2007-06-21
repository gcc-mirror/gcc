! { dg-do compile }
! PR32361 Type declaration to initialize data in named common
      BLOCK DATA
       integer, pointer :: ptr1 => NULL()
       common / T / ptr1
      END
