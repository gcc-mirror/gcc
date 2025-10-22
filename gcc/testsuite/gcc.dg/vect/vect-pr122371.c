/* { dg-do compile } */

struct {
  double lsum;
} AnalyzeSamples_rgData;

float *AnalyzeSamples_curleft;
float AnalyzeSamples_sum_l;
int AnalyzeSamples_i;

void AnalyzeSamples() {
  while (AnalyzeSamples_i--) {
    float l1 = AnalyzeSamples_curleft[1] * AnalyzeSamples_curleft[1],
          l3 = AnalyzeSamples_curleft[3] * AnalyzeSamples_curleft[3],
          sl = l1 + l3;
    AnalyzeSamples_sum_l += sl;
    AnalyzeSamples_curleft += 4;
  }
  AnalyzeSamples_rgData.lsum += AnalyzeSamples_sum_l;
}
