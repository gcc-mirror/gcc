// $HeadURL$
// $Date$
// $Author$

// @author@	Regan Heath <regan@netwin.co.nz>
// @date@	2005-03-30
// @uri@	news:opsof4hwgy23k2f5@nrage.netwin.co.nz

// __DSTRESS_ELINE__ 14

module dstress.nocompile.bug_20050330_A;

template Blah(int a, alias B){
	mixin Blah!(a, B) Foo;
}

int main(){
	int a;
	mixin Blah!(5,a);
	return 0;
}
