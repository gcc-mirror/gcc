! { dg-do compile }
!
! A null dereference fixed
!
! Contributed by Daniel Otero  <canu7@yahoo.es>
!
subroutine foo (rvec)
  TYPE vec_rect_2D_real_acc
    INTEGER :: arr
  END TYPE
  CLASS(vec_rect_2D_real_acc)  rvec

  ASSOCIATE (arr=>rvec%arr)
    call bar(arr*arr)
  end associate
end
