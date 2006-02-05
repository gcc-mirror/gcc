/* { dg-do compile } */
/* { dg-options "-fopenmp -O2 -std=c99" } */

typedef __SIZE_TYPE__ size_t;

typedef struct {
  int _flags;
} FILE;

extern FILE *fopen (__const char *__restrict __filename,
      __const char *__restrict __modes);
extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;
extern int fclose (FILE *__stream);
extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s) ;

extern void *malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;

extern size_t strlen (__const char *__s)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1)));
extern int strncmp (__const char *__s1, __const char *__s2, size_t __n)
     __attribute__ ((__nothrow__)) __attribute__ ((__pure__)) __attribute__ ((__nonnull__ (1, 2)));

extern int __attribute__ ((__nothrow__)) atoi (__const char *__nptr);

extern float cabsf (float _Complex __z) __attribute__ ((__nothrow__)); extern float __cabsf (float _Complex __z) __attribute__ ((__nothrow__));
extern float sqrtf (float __x) __attribute__ ((__nothrow__)); extern float __sqrtf (float __x) __attribute__ ((__nothrow__));

int main(int argc , char * argv[])
{
  int AA, BB, CC;
  AA = 99;
  BB = 99;
  CC = (int)atoi(argv[3]);
  int m,n,s;
  int DD,EE,num_s;


  float _Complex * restrict A;
  A = malloc((AA) * (BB) * (CC) * sizeof(float _Complex));
  int A_slice_stride;
  A_slice_stride = (AA) * (BB) ;


  float * restrict f;
  f = malloc(CC * sizeof(float));



  FILE *fp;

  fp = fopen(argv[1],"rb");
  fread(A,sizeof(float _Complex),AA * BB * CC,fp);
  fclose(fp);

  fp = fopen(argv[2],"rb");
  fread(f,sizeof(float),CC,fp);
  fclose(fp);


  DD = (int)atoi(argv[4]);
  EE = (int)atoi(argv[5]);

  num_s = (EE - DD) + 1;



  float * restrict INPUT;
  INPUT = malloc(4 * 4 * sizeof(float));



  int m_max = 99;
  int n_max = 00;
  float h = 0.1;
  float FF = 10;

  if ((__extension__ (__builtin_constant_p (5) && ((__builtin_constant_p (argv[6]) && strlen (argv[6]) < ((size_t) (5))) || (__builtin_constant_p ("plane") && strlen ("plane") < ((size_t) (5)))) ? __extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (argv[6]) && __builtin_constant_p ("plane") && (__s1_len = strlen (argv[6]), __s2_len = strlen ("plane"), (!((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("plane") + 1) - (size_t)(const void *)("plane") == 1) || __s2_len >= 4)) ? __builtin_strcmp (argv[6], "plane") : (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) && (__s1_len = strlen (argv[6]), __s1_len < 4) ? (__builtin_constant_p ("plane") && ((size_t)(const void *)(("plane") + 1) - (size_t)(const void *)("plane") == 1) ? __builtin_strcmp (argv[6], "plane") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("plane"); register int __result = (((__const unsigned char *) (__const char *) (argv[6]))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (argv[6]))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("plane") && ((size_t)(const void *)(("plane") + 1) - (size_t)(const void *)("plane") == 1) && (__s2_len = strlen ("plane"), __s2_len < 4) ? (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) ? __builtin_strcmp (argv[6], "plane") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (argv[6]); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("plane"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("plane"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("plane"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("plane"))[3]); } } __result; }))) : __builtin_strcmp (argv[6], "plane")))); }) : (__extension__ (__builtin_constant_p (5) && ((__builtin_constant_p (argv[6]) && strlen (argv[6]) < ((size_t) (5))) || (__builtin_constant_p ("plane") && strlen ("plane") < ((size_t) (5)))) ? __extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (argv[6]) && __builtin_constant_p ("plane") && (__s1_len = strlen (argv[6]), __s2_len = strlen ("plane"), (!((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("plane") + 1) - (size_t)(const void *)("plane") == 1) || __s2_len >= 4)) ? __builtin_strcmp (argv[6], "plane") : (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) && (__s1_len = strlen (argv[6]), __s1_len < 4) ? (__builtin_constant_p ("plane") && ((size_t)(const void *)(("plane") + 1) - (size_t)(const void *)("plane") == 1) ? __builtin_strcmp (argv[6], "plane") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("plane"); register int __result = (((__const unsigned char *) (__const char *) (argv[6]))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (argv[6]))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("plane") && ((size_t)(const void *)(("plane") + 1) - (size_t)(const void *)("plane") == 1) && (__s2_len = strlen ("plane"), __s2_len < 4) ? (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) ? __builtin_strcmp (argv[6], "plane") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (argv[6]); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("plane"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("plane"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("plane"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("plane"))[3]); } } __result; }))) : __builtin_strcmp (argv[6], "plane")))); }) : strncmp (argv[6], "plane", 5)))))==0) {
      m_max = INPUT[ ( ((1)-1) + ( ((1)-1)*4 ))];
      n_max = INPUT[ ( ((2)-1) + ( ((1)-1)*4 ))];
      h = INPUT[ ( ((3)-1) + ( ((1)-1)*4 ))];
      FF = INPUT[ ( ((4)-1) + ( ((1)-1)*4 ))];
  }

  if ((__extension__ (__builtin_constant_p (6) && ((__builtin_constant_p (argv[6]) && strlen (argv[6]) < ((size_t) (6))) || (__builtin_constant_p ("sphere") && strlen ("sphere") < ((size_t) (6)))) ? __extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (argv[6]) && __builtin_constant_p ("sphere") && (__s1_len = strlen (argv[6]), __s2_len = strlen ("sphere"), (!((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("sphere") + 1) - (size_t)(const void *)("sphere") == 1) || __s2_len >= 4)) ? __builtin_strcmp (argv[6], "sphere") : (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) && (__s1_len = strlen (argv[6]), __s1_len < 4) ? (__builtin_constant_p ("sphere") && ((size_t)(const void *)(("sphere") + 1) - (size_t)(const void *)("sphere") == 1) ? __builtin_strcmp (argv[6], "sphere") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("sphere"); register int __result = (((__const unsigned char *) (__const char *) (argv[6]))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (argv[6]))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("sphere") && ((size_t)(const void *)(("sphere") + 1) - (size_t)(const void *)("sphere") == 1) && (__s2_len = strlen ("sphere"), __s2_len < 4) ? (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) ? __builtin_strcmp (argv[6], "sphere") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (argv[6]); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("sphere"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("sphere"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("sphere"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("sphere"))[3]); } } __result; }))) : __builtin_strcmp (argv[6], "sphere")))); }) : (__extension__ (__builtin_constant_p (6) && ((__builtin_constant_p (argv[6]) && strlen (argv[6]) < ((size_t) (6))) || (__builtin_constant_p ("sphere") && strlen ("sphere") < ((size_t) (6)))) ? __extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (argv[6]) && __builtin_constant_p ("sphere") && (__s1_len = strlen (argv[6]), __s2_len = strlen ("sphere"), (!((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) || __s1_len >= 4) && (!((size_t)(const void *)(("sphere") + 1) - (size_t)(const void *)("sphere") == 1) || __s2_len >= 4)) ? __builtin_strcmp (argv[6], "sphere") : (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) && (__s1_len = strlen (argv[6]), __s1_len < 4) ? (__builtin_constant_p ("sphere") && ((size_t)(const void *)(("sphere") + 1) - (size_t)(const void *)("sphere") == 1) ? __builtin_strcmp (argv[6], "sphere") : (__extension__ ({ __const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("sphere"); register int __result = (((__const unsigned char *) (__const char *) (argv[6]))[0] - __s2[0]); if (__s1_len > 0 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[1] - __s2[1]); if (__s1_len > 1 && __result == 0) { __result = (((__const unsigned char *) (__const char *) (argv[6]))[2] - __s2[2]); if (__s1_len > 2 && __result == 0) __result = (((__const unsigned char *) (__const char *) (argv[6]))[3] - __s2[3]); } } __result; }))) : (__builtin_constant_p ("sphere") && ((size_t)(const void *)(("sphere") + 1) - (size_t)(const void *)("sphere") == 1) && (__s2_len = strlen ("sphere"), __s2_len < 4) ? (__builtin_constant_p (argv[6]) && ((size_t)(const void *)((argv[6]) + 1) - (size_t)(const void *)(argv[6]) == 1) ? __builtin_strcmp (argv[6], "sphere") : (__extension__ ({ __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (argv[6]); register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("sphere"))[0]; if (__s2_len > 0 && __result == 0) { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("sphere"))[1]); if (__s2_len > 1 && __result == 0) { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("sphere"))[2]); if (__s2_len > 2 && __result == 0) __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("sphere"))[3]); } } __result; }))) : __builtin_strcmp (argv[6], "sphere")))); }) : strncmp (argv[6], "sphere", 6)))))==0) {
      m_max = 181;
      n_max = 361;
      h = INPUT[ ( ((3)-1) + ( ((1)-1)*4 ))];
      FF = INPUT[ ( ((4)-1) + ( ((1)-1)*4 ))];
  }





  float * restrict X;
  X = malloc(m_max * n_max * sizeof(float));


  float * restrict Y;
  Y = malloc(m_max * n_max * sizeof(float));


  float * restrict Z;
  Z = malloc(m_max * n_max * sizeof(float));






  float _Complex * restrict P;
  P = malloc(m_max * n_max * sizeof(float _Complex));


  float _Complex * restrict Ps;
  Ps = malloc((m_max) * (n_max) * (num_s) * sizeof(float _Complex));
  int Ps_slice_stride;
  Ps_slice_stride = (m_max) * (n_max) ;


  float GG;
  float HH;




  for ( n = 1 ; n <= 99 ; n++ ) {
      for ( m = 1 ; m <= 99 ; m++ ) {





	  X[ ( ((m)-1) + ( ((n)-1)*m_max ))] = FF ;
	  Y[ ( ((m)-1) + ( ((n)-1)*m_max ))] = FF ;
	  Z[ ( ((m)-1) + ( ((n)-1)*m_max ))] = FF ;
      }
  }
  int KK = atoi(argv[8]);
  int LL = 3 * KK;


  float * restrict MM;
  MM = malloc(4 * LL * sizeof(float));
  for ( n = 1 ; n <= n_max ; n++) {
      for ( m = 1 ; m <= m_max ; m++) {
	  for ( s = 1 ; s <= num_s ; s++) {
	      Ps[ ( ((m)-1) + (((n)-1)*(m_max)) + (((s)-1)*(Ps_slice_stride)) ) ] = 0.0 + 0.0 * (__extension__ 1.0iF);
	  }
      }
  }


  int liter ;

#pragma omp parallel for private(m,liter,s)
  for ( n = 1 ; n <= n_max ; n++) {
      for ( m = 1 ; m <= m_max ; m++) {
	  for ( liter = 1 ; liter <= KK ; liter++ ) {
	      for ( s = 1 ; s <= num_s ; s++) {


		  int LM_column;
		  float NN[4];
		  float OO[4];
		  float PP[4];
		  float QQ[4];
		  float k;
		  int s_index;
		  float RR;

		  s_index = s + (DD -1);
		  RR = f[ ( (s_index)-1) ];
		  k = 99.0;

		  NN[1 -1] = X[ ( ((m)-1) + ( ((n)-1)*m_max ))];
		  NN[2 -1] = Y[ ( ((m)-1) + ( ((n)-1)*m_max ))];
		  NN[3 -1] = Z[ ( ((m)-1) + ( ((n)-1)*m_max ))];
		  NN[4 -1] = 1.0;

		  LM_column = ((liter -1) * 3) + 1;
		  OO[1 -1] = MM[ ( ((1)-1) + ( ((LM_column)-1)*4 ))];
		  OO[2 -1] = MM[ ( ((2)-1) + ( ((LM_column)-1)*4 ))];
		  OO[3 -1] = MM[ ( ((3)-1) + ( ((LM_column)-1)*4 ))];
		  OO[4 -1] = MM[ ( ((4)-1) + ( ((LM_column)-1)*4 ))];

		  LM_column = ((liter -1) * 3) + 2;
		  PP[1 -1] = MM[ ( ((1)-1) + ( ((LM_column)-1)*4 ))];
		  PP[2 -1] = MM[ ( ((2)-1) + ( ((LM_column)-1)*4 ))];
		  PP[3 -1] = MM[ ( ((3)-1) + ( ((LM_column)-1)*4 ))];
		  PP[4 -1] = MM[ ( ((4)-1) + ( ((LM_column)-1)*4 ))];

		  LM_column = ((liter -1) * 3) + 3;
		  QQ[1 -1] = MM[ ( ((1)-1) + ( ((LM_column)-1)*4 ))];
		  QQ[2 -1] = MM[ ( ((2)-1) + ( ((LM_column)-1)*4 ))];
		  QQ[3 -1] = MM[ ( ((3)-1) + ( ((LM_column)-1)*4 ))];
		  QQ[4 -1] = MM[ ( ((4)-1) + ( ((LM_column)-1)*4 ))];

	      }
	  }
      }
  }


#pragma omp parallel for private(m)
  for ( n = 1 ; n <= n_max ; n++) {
      for ( m = 1 ; m <= m_max ; m++) {



	  int s;
	  float SSS;
	  float f1,f2,p1,p2,TT,h,FFF;
	  SSS = 0.0;
	  for ( s = 2 ; s <= num_s ; s++) {
	      f1 = f[ ( ((s-1) + (DD - 1))-1) ];
	      f2 = f[ ( ((s) + (DD - 1))-1) ];
	      p1 = cabsf(Ps[ ( ((m)-1) + (((n)-1)*(m_max)) + ((((s-1))-1)*(Ps_slice_stride)) ) ]) ;
	      p2 = cabsf(Ps[ ( ((m)-1) + (((n)-1)*(m_max)) + (((s)-1)*(Ps_slice_stride)) ) ]) ;

	      h = f2 - f1;

	      FFF = (f1 + f2) / 2.0;


	      TT = (1.0 / sqrtf(2.0)) * (((h * p1) + (0.5 * h * (p2 - p1))) * (1.0 / FFF));

	      SSS += TT;

	  }

	  P[ ( ((m)-1) + ( ((n)-1)*m_max ))] = SSS + ((__extension__ 1.0iF) * 0.0);
      }
  }





  fp = fopen(argv[10],"wb");
  fwrite(X,sizeof(float),m_max * n_max,fp);
  fclose(fp);

  fp = fopen(argv[11],"wb");
  fwrite(Y,sizeof(float),m_max * n_max,fp);
  fclose(fp);

  fp = fopen(argv[12],"wb");
  fwrite(Z,sizeof(float),m_max * n_max,fp);
  fclose(fp);

  fp = fopen(argv[13],"wb");
  fwrite(P,sizeof(float _Complex),m_max * n_max,fp);
  fclose(fp);



  return(0);
}
