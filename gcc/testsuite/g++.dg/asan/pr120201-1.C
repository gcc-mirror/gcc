// { dg-do run }
// { dg-options "-O3 -std=c++23 -fsanitize=address" }

using size_t = decltype (sizeof 0);

float* shaderLightData;

using ShaderShadowTransform = float[4z * 4z * 6z];
using Transform = float[4 * 4];

__attribute__ ((noipa))
#if defined(__i386__) || defined(__x86_64__)
__attribute__ ((target ("arch=x86-64-v4")))
#endif
static void
do_test (void)
{
  int lightCount = 3;
  ShaderShadowTransform* shaderShadowData = new ShaderShadowTransform[lightCount];
  for (int index = 0; index < lightCount; index++)
    {
      Transform transforms[6];

      const size_t matSize = 4z * 4z;
      float* transformBlockStart = shaderShadowData[index];
      for (int face = 0; face < 6; face++)
	__builtin_memcpy(transformBlockStart + (matSize * face),
			 &transforms[face][0], matSize * sizeof(float));
  }

  delete [] shaderShadowData;
}

int
main (void)
{
#if defined(__i386__) || defined(__x86_64__)
  if (__builtin_cpu_supports ("x86-64-v4"))
#endif
    do_test ();
  return 0;
}
