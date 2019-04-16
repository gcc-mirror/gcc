/* { dg-do run { target { vsx_hw } } } */
/* { dg-options "-O2 -mvsx -mdejagnu-cpu=power7" } */

#include <altivec.h> // vector

#ifdef DEBUG
#include <stdio.h>
#endif

#define VBC   0
#define VSC   1
#define VUC   2
#define VBS   3
#define VSS   4
#define VUS   5
#define VBI   6
#define VI    7
#define VUI   8
#define VLLB  9
#define VLLI  10
#define VLLUI 11
#define VF    12
#define VD    13

union vector_value
{
  vector bool char vbc;
  vector signed char vsc;
  vector unsigned char vuc;
  vector bool short vbs;
  vector signed short vss;
  vector unsigned short vus;
  vector bool int vbi;
  vector signed int vi;
  vector unsigned int vui;
  vector bool long long vllb;
  vector long long signed int vlli;
  vector long long unsigned int vllui;
  vector float vf;
  vector double vd;
} vec_element;

struct vector_struct
{
  int vector_id;
  int element_size;  // element size in bytes
  union vector_value vec;
} vec;

void abort (void);

void test_results(struct vector_struct *vec_result,
		  struct vector_struct *vec_expected)
{
  int i;
  int num_elements;
  if (vec_result->element_size != vec_expected->element_size)
#ifdef DEBUG
    printf("vec_result->element_size != vec_expected->element_size\n");
#else
    abort();
#endif

  if (vec_result->vector_id != vec_expected->vector_id)
#ifdef DEBUG
    printf("vec_result->vector_id != vec_expected->vector_id\n");
#else
    abort();
#endif

   num_elements = 16 / vec_result->element_size;

  for (i = 0; i<num_elements; i++) {
    switch (vec_result->vector_id) {
      case VBC:
	if (vec_result->vec.vbc[i] != vec_expected->vec.vbc[i])
	  {
#ifdef DEBUG
	     printf("vec_result->vec.vbc[%d] (%d) != ",
		    i, vec_result->vec.vbc[i]);
	     printf("vec_expected->vec.vbc[%d] (%d)\n",
		    i, vec_expected->vec.vbc[i]);
#else
	     abort();
#endif
	  }
	break;

      case VSC:
	if (vec_result->vec.vsc[i] != vec_expected->vec.vsc[i])
	  {
#ifdef DEBUG
	     printf("vec_result->vec.vsc[%d] (%d) != ",
		    i, vec_result->vec.vsc[i]);
	     printf("vec_expected->vec.vsc[%d] (%d)\n",
		    i, vec_expected->vec.vsc[i]);
#else
	     abort();
#endif
	  }
	break;

      case VUC:
	if (vec_result->vec.vuc[i] != vec_expected->vec.vuc[i])
	  {
#ifdef DEBUG
	     printf("vec_result->vec.vuc[%d] (%d) != ",
		    i, vec_result->vec.vuc[i]);
	     printf("vec_expected->vec.vuc[%d] (%d)\n",
		    i, vec_expected->vec.vuc[i]);
#else
	     abort();
#endif
	  }
	break;

      case VBS:
	if (vec_result->vec.vbs[i] != vec_expected->vec.vbs[i])
	  {
#ifdef DEBUG
	     printf("vec_result->vec.vbs[%d] (%d) != ",
		    i, vec_result->vec.vbs[i]);
	     printf("vec_expected->vec.vbs[%d] (%d)\n",
		    i, vec_expected->vec.vbs[i]);
#else
	    abort();
#endif
	  }
	break;

      case VSS:
	if (vec_result->vec.vss[i] != vec_expected->vec.vss[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vss[%d] (%d) != ",
		    i, vec_result->vec.vss[i]);
	    printf("vec_expected->vec.vss[%d] (%d)\n",
		    i, vec_expected->vec.vss[i]);
#else
	    abort();
#endif
	  }
	break;

      case VUS:
	if (vec_result->vec.vus[i] != vec_expected->vec.vus[i])
	  {
#ifdef DEBUG
	     printf("vec_result->vec.vus[%d] (%d) != ",
		    i, vec_expected->vec.vus[i]);
	     printf("vec_expected->vec.vus[%d] (%d)\n",
		    i, vec_expected->vec.vus[i]);
#else
	     abort();
#endif
	  }
	break;

      case VBI:
	if (vec_result->vec.vbi[i] != vec_expected->vec.vbi[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vbi[%d] (%d) != ",
		   i, vec_result->vec.vbi[i]);
	    printf("vec_expected->vec.vbi[%d] (%d)\n",
		   i, vec_expected->vec.vbi[i]);
#else
	    abort();
#endif
	  }
	break;

      case VI:
	if (vec_result->vec.vi[i] != vec_expected->vec.vi[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vi[%d] (%d) != ",
		   i, vec_result->vec.vi[i]);
	    printf("vec_expected->vec.vi[%d] (%d)\n",
		   i, vec_expected->vec.vi[i]);
#else
	    abort();
#endif
	  }
	break;

      case VUI:
	if (vec_result->vec.vui[i] != vec_expected->vec.vui[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vui[%d] (%u) != ",
		   i, vec_result->vec.vui[i]);
	    printf("vec_expected->vec.vui[%u] (%d)\n",
		   i, vec_expected->vec.vui[i]);
#else
	    abort();
#endif
	  }
	break;

      case VLLB:
	if (vec_result->vec.vllb[i] != vec_expected->vec.vllb[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vllb[%d] (%lld != ",
		   i, vec_result->vec.vllb[i]);
	    printf("vec_expected->vec.vllb[%lld] (%d)\n",
		   i, vec_expected->vec.vllb[i]);
#else
	    abort();
#endif
	  }
	break;

      case VLLI:
	if (vec_result->vec.vlli[i] != vec_expected->vec.vlli[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vlli[%d] (%d) != ",
		   i, vec_result->vec.vlli[i]);
	    printf("vec_expected->vec.vlli[%d] (%d)\n",
		   i, vec_expected->vec.vlli[i]);
#else
	    abort();
#endif
	  }
	break;

      case VLLUI:
	if (vec_result->vec.vllui[i] != vec_expected->vec.vllui[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vllui[%d] (%llu) != ",
		   i, vec_result->vec.vllui[i]);
	    printf("vec_expected->vec.vllui[%d] (%llu)\n",
		   i, vec_expected->vec.vllui[i]);
#else
	    abort();
#endif
	  }
	break;

      case VF:
	if (vec_result->vec.vf[i] != vec_expected->vec.vf[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vf[%d] (%f) != ",
		   i, vec_result->vec.vf[i]);
	    printf("vec_expected->vec.vf[%d] (%f)\n",
		   i, vec_expected->vec.vf[i]);
#else
	    abort();
#endif
	  }
	break;

      case VD:
	if (vec_result->vec.vd[i] != vec_expected->vec.vd[i])
	  {
#ifdef DEBUG
	    printf("vec_result->vec.vd[%d] (%f) != ",
		   i, vec_result->vec.vd[i]);
	    printf("vec_expected->vec.vd[%d] (%f)\n",
		   i, vec_expected->vec.vd[i]);
#else
	    abort();
#endif
	  }
	   break;

      default:
#ifdef DEBUG
	printf("Unknown case.\n");
#else
	abort();
#endif
      }
   }
}

int main()
{
  int i;
  struct vector_struct vec_src, vec_expected, vec_result;

  vec_src.vec.vbc = (vector bool char){ 0, 1, 0, 0, 1, 1, 0, 0,
					0, 1, 1, 1, 0, 0, 0, 0 };
  vec_expected.vec.vbc = (vector bool char){ 0, 0, 0, 0, 1, 1, 1, 0,
					     0, 0, 1, 1, 0, 0, 1, 0 };
  vec_result.element_size = vec_expected.element_size = 1;
  vec_result.vector_id = vec_expected.vector_id = VBC;
  vec_result.vec.vbc = vec_reve (vec_src.vec.vbc);
  test_results(&vec_result, &vec_expected);

  vec_src.vec.vsc = (vector signed char){ 0, 1, -2, -3, 4, 5, -6, -7, 8,
					  9, -10, -11, 12, 13, -14, -15 };
  vec_expected.vec.vsc = (vector signed char){ -15, -14, 13, 12, -11, -10,
					       9, 8, -7, -6, 5, 4, -3, -2,
					       1, 0 };
  vec_result.element_size = vec_expected.element_size = 1;
  vec_result.vector_id = vec_expected.vector_id = VSC;
  vec_result.vec.vsc = vec_reve (vec_src.vec.vsc);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vuc = (vector unsigned char){ 10, 11, 12, 13, 14, 15, 16, 17,
					    18, 19, 20, 21, 22, 23, 24, 25 };
  vec_expected.vec.vuc = (vector unsigned char){ 25, 24, 23, 22, 21, 20,
						 19, 18, 17, 16, 15, 14, 13,
						 12, 11, 10 };
  vec_result.element_size = vec_expected.element_size = 1;
  vec_result.vector_id = vec_expected.vector_id = VUC;
  vec_result.vec.vuc = vec_reve (vec_src.vec.vuc);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vbs = (vector bool short){ 0, 0, 1, 1, 0, 1, 0, 1 };
  vec_expected.vec.vbs = (vector bool short){ 1, 0, 1, 0, 1, 1, 0, 0 };
  vec_result.element_size = vec_expected.element_size = 2;
  vec_result.vector_id = vec_expected.vector_id = VBS;
  vec_result.vec.vbs = vec_reve (vec_src.vec.vbs);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vss = (vector signed short){ -1, -2, 3, 4, -5, -6, 7, 8 };
  vec_expected.vec.vss = (vector signed short){ 8, 7, -6, -5, 4, 3, -2, -1 };
  vec_result.element_size = vec_expected.element_size = 2;
  vec_result.vector_id = vec_expected.vector_id = VSS;
  vec_result.vec.vss = vec_reve (vec_src.vec.vss);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vus = (vector unsigned short){ 11, 22, 33, 44, 55, 66, 77, 88 };
  vec_expected.vec.vus = (vector unsigned short){ 88, 77, 66, 55,
						  44, 33, 22, 11 };
  vec_result.element_size = vec_expected.element_size = 2;
  vec_result.vector_id = vec_expected.vector_id = VUS;
  vec_result.vec.vus = vec_reve (vec_src.vec.vus);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vbi = (vector bool int){ 0, 1, 1, 1 };
  vec_expected.vec.vbi = (vector bool int){ 1, 1, 1, 0 };
  vec_result.element_size = vec_expected.element_size = 4;
  vec_result.vector_id = vec_expected.vector_id = VBI;
  vec_result.vec.vbi = vec_reve (vec_src.vec.vbi);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vi = (vector signed int){ -1, 3, -5, 1234567 };
  vec_expected.vec.vi = (vector signed int){1234567, -5, 3, -1};
  vec_result.element_size = vec_expected.element_size = 4;
  vec_result.vector_id = vec_expected.vector_id = VI;
  vec_result.vec.vi = vec_reve (vec_src.vec.vi);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vui = (vector unsigned int){ 9, 11, 15, 2468013579 };
  vec_expected.vec.vui = (vector unsigned int){2468013579, 15, 11, 9};
  vec_result.element_size = vec_expected.element_size = 4;
  vec_result.vector_id = vec_expected.vector_id = VUI;
  vec_result.vec.vui = vec_reve (vec_src.vec.vui);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vllb = (vector bool long long ){ 0, 1 };
  vec_expected.vec.vllb = (vector bool long long){1, 0};
  vec_result.element_size = vec_expected.element_size = 8;
  vec_result.vector_id = vec_expected.vector_id = VLLB;
  vec_result.vec.vllb = vec_reve (vec_src.vec.vllb);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vlli = (vector long long int){ -12, -12345678901234 };
  vec_expected.vec.vlli = (vector long long int){-12345678901234, -12};
  vec_result.element_size = vec_expected.element_size = 8;
  vec_result.vector_id = vec_expected.vector_id = VLLI;
  vec_result.vec.vlli = vec_reve (vec_src.vec.vlli);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vllui = (vector unsigned long long int){ 102, 9753108642 };
  vec_expected.vec.vllui = (vector unsigned long long int){9753108642, 102};
  vec_result.element_size = vec_expected.element_size = 8;
  vec_result.vector_id = vec_expected.vector_id = VLLUI;
  vec_result.vec.vllui = vec_reve (vec_src.vec.vllui);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vf = (vector float){ -21., 3.5, -53., 78. };
  vec_expected.vec.vf = (vector float){78., -53, 3.5, -21};
  vec_result.element_size = vec_expected.element_size = 4;
  vec_result.vector_id = vec_expected.vector_id = VF;
  vec_result.vec.vf = vec_reve (vec_src.vec.vf);
  test_results (&vec_result, &vec_expected);

  vec_src.vec.vd = (vector double){ 34.0, 97.0 };
  vec_expected.vec.vd = (vector double){97.0, 34.0};
  vec_result.element_size = vec_expected.element_size = 8;
  vec_result.vector_id = vec_expected.vector_id = VD;
  vec_result.vec.vd = vec_reve (vec_src.vec.vd);
  test_results (&vec_result, &vec_expected);
}
