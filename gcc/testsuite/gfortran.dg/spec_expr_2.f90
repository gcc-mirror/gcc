! { dg-do compile }
! PR 22273: Allow INTENT(OUT) dummy:s as arguments to LEN() in specification
! expr:s
subroutine lecligne (ligne)
    character(len=*), intent(out) :: ligne
    character(len=len(ligne)) :: comment
end subroutine lecligne
