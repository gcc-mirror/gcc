
// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-require-effective-target vect_double }
// { dg-options "-O3 -fdump-tree-vect-all" }

#include <cstdlib>
#include <cmath>
#include <iostream>
void DoIfClause(double *l, double *r, const double *m2, const double *m1,
        const double *c, const double *p1, const double *p2, int istart, int iend)
{
    constexpr double w5alpha[3][3] = {{1.0 / 3.0, -7.0 / 6.0, 11.0 / 6.0},
        {-1.0 / 6.0, 5.0 / 6.0, 1.0 / 3.0},
        {1.0 / 3.0, 5.0 / 6.0, -1.0 / 6.0}};
    constexpr double w5gamma[3] = {0.1, 0.6, 0.3};
    constexpr double eps = 1e-100;
    constexpr double thirteen_thirds = 13.0 / 3.0;

    for (int i = istart; i <= iend; ++i) {
        const double q0 = m2[i];
        const double q1 = m1[i];
        const double q2 = c[i];
        const double q3 = p1[i];
        const double q4 = p2[i];
        double &ql = l[i];
        double &qr = r[i];
        double a = q0 - 2 * q1 + q2;
        double b = q0 - 4.0 * q1 + 3.0 * q2;
        double beta0 = thirteen_thirds * a * a + b * b + eps;
        a = q1 - 2.0 * q2 + q3;
        b = q3 - q1;
        double beta1 = thirteen_thirds * a * a + b * b + eps;
        a = q2 - 2.0 * q3 + q4;
        b = q4 - 4.0 * q3 + 3.0 * q2;
        double beta2 = thirteen_thirds * a * a + b * b + eps;
        const double tau5 = std::abs(beta2 - beta0);

        beta0 = (beta0 + tau5) / beta0;
        beta1 = (beta1 + tau5) / beta1;
        beta2 = (beta2 + tau5) / beta2;

        double w0 = w5gamma[0] * beta0 + eps;
        double w1 = w5gamma[1] * beta1 + eps;
        double w2 = w5gamma[2] * beta2 + eps;
        double wsum = 1.0 / (w0 + w1 + w2);
        ql = w0 * (w5alpha[0][0] * q0 + w5alpha[0][1] * q1 + w5alpha[0][2] * q2);
        ql += w1 * (w5alpha[1][0] * q1 + w5alpha[1][1] * q2 + w5alpha[1][2] * q3);
        ql += w2 * (w5alpha[2][0] * q2 + w5alpha[2][1] * q3 + w5alpha[2][2] * q4);
        ql *= wsum;
        const double alpha_l = 3.0 * wsum * w0 * w1 * w2 /
            (w5gamma[2] * w0 * w1 + w5gamma[1] * w0 * w2 +
             w5gamma[0] * w1 * w2) +
            eps;

        w0 = w5gamma[0] * beta2 + eps;
        w1 = w5gamma[1] * beta1 + eps;
        w2 = w5gamma[2] * beta0 + eps;
        wsum = 1.0 / (w0 + w1 + w2);
        qr = w0 * (w5alpha[0][0] * q4 + w5alpha[0][1] * q3 + w5alpha[0][2] * q2);
        qr += w1 * (w5alpha[1][0] * q3 + w5alpha[1][1] * q2 + w5alpha[1][2] * q1);
        qr += w2 * (w5alpha[2][0] * q2 + w5alpha[2][1] * q1 + w5alpha[2][2] * q0);
        qr *= wsum;
        const double alpha_r = 3.0 * wsum * w0 * w1 * w2 /
            (w5gamma[2] * w0 * w1 + w5gamma[1] * w0 * w2 +
             w5gamma[0] * w1 * w2) +
            eps;

        double dq = q3 - q2;
        {
            const double dm = q2 - q1;
            const double dp = dq;
            const double dc = (dm * dp > 0.0) * 0.5 * (dm + dp);
            dq = 0.5 * std::copysign(
                    std::min(std::fabs(dc),
                        2.0 * std::min(std::fabs(dm), std::fabs(dp))),
                    dc);
        }

        const double alpha_lin = 2.0 * alpha_l * alpha_r / (alpha_l + alpha_r);
        ql = alpha_lin * ql + (1.0 - alpha_lin) * (q2 + dq);
        qr = alpha_lin * qr + (1.0 - alpha_lin) * (q2 - dq);
    }
}
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
