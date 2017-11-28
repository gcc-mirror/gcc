      ! { dg-do run }
      ! { dg-options "-fdec-structure" }
      !
      ! PR fortran/82511
      !
      ! Verify that structure variables with UNION components
      ! are accepted in an I/O-list READ.
      !
      implicit none

      structure /s/
        union
          map
            character(16) :: c16_1
          end map
          map
            character(16) :: c16_2
          end map
        end union
      end structure

      record /s/ r
      character(32) :: instr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^"

      r.c16_1 = '                '
      r.c16_2 = '                '
      ! The record r shall be treated as if its components are listed:
      ! read(...) r.c16_1, r.c16_2
      ! This shall correspond to the formatted read of A16,A16
      read(instr, '(A16,A16)') r

      ! r.c16_1 and r.c16_2 are in a union, thus share the same memory
      ! and the first 16 bytes of instr are overwritten
      if ( r.c16_1 .ne. instr(17:32) .or. r.c16_2 .ne. instr(17:32) ) then
        call abort()
      endif

      end
