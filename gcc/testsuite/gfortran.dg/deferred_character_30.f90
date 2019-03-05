! { dg-do compile }
!
! Fix a regression introduced by the patch for PR70149.
!
    character (:), pointer :: ptr => NULL() ! The NULL () caused an ICE.
    character (6), target :: tgt = 'lmnopq'
    ptr => tgt
    print *, len (ptr), ptr
end
