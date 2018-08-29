! { dg-do run }
! { dg-options "-fdec-structure" }
!
! Test STRUCTUREs which share names with variables.
!

subroutine aborts (s)
  character(*), intent(in) :: s
  print *, s
  STOP 1
end subroutine

! Special regression where shared names within a module caused an ICE
! from gfc_get_module_backend_decl
module dec_structure_5m
  structure /s6/
    integer i
  end structure

  record /s6/ s6
end module

program dec_structure_5
  use dec_structure_5m

  structure /s7/
    real r
  end structure

  record /s7/ s7(3)

  s6.i = 0
  s7(1).r = 1.0
  s7(2).r = 2.0
  s7(3).r = 3.0

  if (s6.i .ne. 0) then
    call aborts("s6.i")
  endif

  if (s7(1).r .ne. 1.0) then
    call aborts("s7(1).r")
  endif

  if (s7(2).r .ne. 2.0) then
    call aborts("s7(2).r")
  endif

end
