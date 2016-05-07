! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test nested UNIONs.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  call abort()
end subroutine

! Nested unions
structure /s4/
  union ! U0                ! rax
    map
      integer(8) rx
    end map
    map
      integer(4) rh         ! rah
      union ! U1
        map
          integer(4) rl     ! ral
        end map
        map
          integer(4) ex     ! eax
        end map
        map
          integer(2) eh     ! eah
          union ! U2
            map
              integer(2) el ! eal
            end map
            map
              integer(2) x  ! ax
            end map
            map
              integer(1) h  ! ah
              integer(1) l  ! al
            end map
          end union
        end map
      end union
    end map
  end union
end structure
record /s4/ r4


! Nested unions
r4.rx     =     z'7A7B7CCC7FFFFFFF'
if ( r4.rx .ne. z'7A7B7CCC7FFFFFFF' ) call aborts ("rax")
if ( r4.rh .ne.         z'7FFFFFFF' ) call aborts ("rah")
if ( r4.rl .ne. z'7A7B7CCC'         ) call aborts ("ral")
if ( r4.ex .ne. z'7A7B7CCC'         ) call aborts ("eax")
if ( r4.eh .ne.     z'7CCC'         ) call aborts ("eah")
if ( r4.el .ne. z'7A7B'             ) call aborts ("eal")
if ( r4.x  .ne. z'7A7B'             ) call aborts ("ax")
if ( r4.h  .ne.   z'7B'             ) call aborts ("ah")
if ( r4.l  .ne. z'7A'               ) call aborts ("al")

end
