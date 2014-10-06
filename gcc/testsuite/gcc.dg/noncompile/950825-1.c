int
main()
{
return (struct x) {{y: 0}};   /* { dg-error "extra|near|excess|incompatible|invalid" } */
}
