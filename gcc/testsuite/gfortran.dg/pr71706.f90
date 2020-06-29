! { dg-do compile }
! { dg-options "-fcoarray=lib -fcheck=all -fdefault-integer-8" }
! PR fortran/71706 - ICE on using sync images with -fcheck=bounds

program p
  integer, volatile :: me = 1
  sync images (me)
  sync images (int (me, 2))
  sync images (int (me, 8))
end
