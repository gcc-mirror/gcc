! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test nested UNIONs.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

! Nested unions
structure /s4/
  union ! U0                ! rax
    map
      character(16) rx
    end map
    map
      character(8) rh         ! rah
      union ! U1
        map
          character(8) rl     ! ral
        end map
        map
          character(8) ex     ! eax
        end map
        map
          character(4) eh     ! eah
          union ! U2
            map
              character(4) el ! eal
            end map
            map
              character(4) x  ! ax
            end map
            map
              character(2) h  ! ah
              character(2) l  ! al
            end map
          end union
        end map
      end union
    end map
  end union
end structure
record /s4/ r4


! Nested unions
r4.rx     =     'AAAAAAAA.BBB.C.D'

if ( r4.rx .ne. 'AAAAAAAA.BBB.C.D' ) call aborts ("rax")
if ( r4.rh .ne. 'AAAAAAAA'         ) call aborts ("rah")
if ( r4.rl .ne.         '.BBB.C.D' ) call aborts ("ral")
if ( r4.ex .ne.         '.BBB.C.D' ) call aborts ("eax")
if ( r4.eh .ne.         '.BBB'     ) call aborts ("eah")
if ( r4.el .ne.             '.C.D' ) call aborts ("eal")
if ( r4.x  .ne.             '.C.D' ) call aborts ("ax")
if ( r4.h  .ne.             '.C'   ) call aborts ("ah")
if ( r4.l  .ne.               '.D' ) call aborts ("al")

end
