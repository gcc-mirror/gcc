__complex__ float
func (__complex__ float x)
{
    if (__real__ x == 0.0)
	return 1.0;
    else
	return 0.0;
}
