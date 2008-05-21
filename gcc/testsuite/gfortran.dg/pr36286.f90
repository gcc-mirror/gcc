! { dg-do compile }
! { dg-options "-O1 -ftree-loop-linear" }
! PR tree-optimization/36286

program test_count
    integer, dimension(2,3) :: a, b
    a = reshape( (/ 1, 3, 5, 2, 4, 6 /), (/ 2, 3 /))
    b = reshape( (/ 0, 3, 5, 7, 4, 8 /), (/ 2, 3 /))
    print '(3l6)', a.ne.b
    print *, a(1,:).ne.b(1,:)
    print *, a(2,:).ne.b(2,:)
    print *, count(a.ne.b)
end program test_count

