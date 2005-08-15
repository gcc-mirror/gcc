template <class T> struct PCVector2
{ // { dg-error "" }
    template <class T2> PCVector2(const PCVector2<T> &cv) ;

    PCVector2<T> operator- (const PCVector2<T> &ov) const 
	{ 
	  return PCVector2<T>(ov.xFIELD, ov.yFIELD); // { dg-error "" }
	}

    T xFIELD, yFIELD;
};

void findIntersection( PCVector2<double>& p0, PCVector2<double>& p1);


void findIntersection( PCVector2<double>& p0, PCVector2<double>& p1)
{
    PCVector2<double> e = p1 - p0;	// { dg-error "" }
}
