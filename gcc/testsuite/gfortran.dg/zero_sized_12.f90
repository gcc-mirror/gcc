! { dg-do compile }
! PR 65248 - this used to ICE. Test case by Tobias Burnus.

program main
  
! C7110  (R770) If type-spec is omitted, each ac-value expression in the
! array-constructor shall have the same declared type and kind type parameters

! Should be fine as there is either no or only one ac-value:
print *, [[integer ::],[real::]]
print *, [[integer ::],[real::], [1], [real ::]]
print *, [[integer ::],[real::], ["ABC"], [real ::]] // "ABC"
print *, [integer :: [integer ::],[real::]]

! OK - accepted
print *, [integer :: [1],[1.0]]
end
