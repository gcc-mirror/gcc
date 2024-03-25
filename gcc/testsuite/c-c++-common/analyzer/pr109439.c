int would_like_only_oob (int i)
{
    int arr[] = {1,2,3,4,5,6,7};
    arr[10] = 9; /* { dg-warning "stack-based buffer overflow" } */
    arr[11] = 15; /* { dg-warning "stack-based buffer overflow" } */
    int y1 = arr[9]; /* { dg-warning "stack-based buffer over-read" } */
            /* { dg-bogus "use of uninitialized value" "" { target *-*-* } .-1 } */

    arr[18] = 15; /* { dg-warning "stack-based buffer overflow" } */
    
    return y1;
}
