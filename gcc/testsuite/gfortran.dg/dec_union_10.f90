! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! Check for regression where gfc_compare_union_types wasn't properly guarded
! against empty unions.
!

subroutine sub1(r)
  structure /s/
    union
    end union
  end structure
  record /s/ r
end subroutine

subroutine sub2()
  structure /s/
    union
    end union
  end structure
  record /s/ r
  call sub1(r)
end subroutine

call sub2()

end
