// GROUPS passed code-generation
// Check that type float parameters can be correctly passed to
// methods.

extern "C" int printf (const char *, ...); 

class tres_floats {
	float ff1;
	float ff2;
	float ff3;
public:
	tres_floats (float f1, float f2, float f3);
	float get_f1 ();
	float get_f2 ();
	float get_f3 ();
};

float v1 = 1.2345;
float v2 = 3.14159;
float v3 = 0.707;

int main ()
{
	tres_floats tf (v1, v2, v3);

	if ((tf.get_f1() != v1) || (tf.get_f2() != v2) || (tf.get_f3() != v3))
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");

	return 0;
}

tres_floats::tres_floats (float f1, float f2, float f3)
{
	ff1 = f1;
	ff2 = f2;
	ff3 = f3;
}

float tres_floats::get_f1 ()
{
	return ff1;
}

float tres_floats::get_f2 ()
{
	return ff2;
}

float tres_floats::get_f3 ()
{
	return ff3;
}
