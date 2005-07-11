! tests FORALL statements with a mask
program forall_7
  real, dimension (5, 5, 5, 5) :: a, b, c, d

  a (:, :, :, :) = 4
  forall (i = 1:5)
    a (i, i, 6 - i, i) = 7
  end forall
  forall (i = 1:5)
    a (i, 6 - i, i, i) = 7
  end forall
  forall (i = 1:5)
    a (6 - i, i, i, i) = 7
  end forall
  forall (i = 1:5:2)
    a (1, 2, 3, i) = 0
  end forall

  b = a
  c = a
  d = a

  forall (i = 1:5, j = 1:5, k = 1:5, ((a (i, j, k, i) .gt. 6) .or. (a (i, j, k, j) .gt. 6)))
    forall (l = 1:5, a (1, 2, 3, l) .lt. 2)
      a (i, j, k, l) = i - j + k - l + 0.5
    end forall
  end forall

  forall (l = 1:5, b (1, 2, 3, l) .lt. 2)
    forall (i = 1:5, j = 1:5, k = 1:5, ((b (i, j, k, i) .gt. 6) .or. (b (i, j, k, j) .gt. 6)))
      b (i, j, k, l) = i - j + k - l + 0.5
    end forall
  end forall

  forall (i = 1:5, j = 1:5, k = 1:5, ((c (i, j, k, i) .gt. 6) .or. (c (i, j, k, j) .gt. 6)))
    forall (l = 1:5, c (1, 2, 3, l) .lt. 2)
      c (i, j, k, l) = i - j + k - l + 0.5 + c (l, k, j, i)
    end forall
  end forall

  forall (l = 1:5, d (1, 2, 3, l) .lt. 2)
    forall (i = 1:5, j = 1:5, k = 1:5, ((d (i, j, k, i) .gt. 6) .or. (d (i, j, k, j) .gt. 6)))
      d (i, j, k, l) = i - j + k - l + 0.5 + d (l, k, j, i)
    end forall
  end forall

  do i = 1, 5
    do j = 1, 5
      do k = 1, 5
	do l = 1, 5
	  r = 4
	  if ((i == j .and. k == 6 - i) .or. (i == k .and. j == 6 - i)) then
	    if (l /= 2 .and. l /= 4) then
	      r = 1
	    elseif (l == i) then
	      r = 7
	    end if
	  elseif (j == k .and. i == 6 - j) then
	    if (l /= 2 .and. l /= 4) then
	      r = 1
	    elseif (l == j) then
	      r = 7
	    end if
	  elseif (i == 1 .and. j == 2 .and. k == 3 .and. l /= 2 .and. l /= 4) then
	    r = 0
	  end if
	  s = r
	  if (r == 1) then
	    r = i - j + k - l + 0.5
	    if (((l == k .and. j == 6 - l) .or. (l == j .and. k == 6 - l)) .and. (i == l)) then
	      s = r + 7
	    elseif (k == j .and. l == 6 - k .and. i == k) then
	      s = r + 7
	    elseif (l /= 1 .or. k /= 2 .or. j /= 3 .or. i == 2 .or. i == 4) then
	      s = r + 4
	    else
	      s = r
	    end if
	  end if
	  if (a (i, j, k, l) /= r) call abort ()
	  if (c (i, j, k, l) /= s) call abort ()
	end do
      end do
    end do
  end do

  if (any (a /= b .or. c /= d)) call abort ()
end
