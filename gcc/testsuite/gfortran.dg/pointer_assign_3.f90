! { dg-do compile }
! PR32360 Won't compile 'data ptr1 /null ()/' when ptr1 has pointer attribute.
      integer, pointer :: ptr1
      data ptr1 /NULL()/
      end

