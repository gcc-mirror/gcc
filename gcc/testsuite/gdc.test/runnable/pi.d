// PERMUTE_ARGS:
// EXECUTE_ARGS: 1000

import core.stdc.stdio;
import core.stdc.time;

const int LONG_TIME=4000;

byte[] p;
byte[] t;
int q;

int main(char[][] args)
{
        time_t startime, endtime;
        int i;

        if (args.length == 2) {
                sscanf(&args[1][0],"%d",&q);
        } else {
                printf("Usage: pi [precision]\n");
                return 1;
        }

        if (q < 0)
        {
                printf("Precision was too low, running with precision of 0.\n");
                q = 0;
        }

        if (q > LONG_TIME)
        {
            printf("Be prepared to wait a while...\n");
        }

        // Compute one more digit than we display to compensate for rounding
        q++;

        p.length = q + 1;
        t.length = q + 1;

        /* compute pi */

        time(&startime);
        arctan(2);
        arctan(3);
        mul4();
        time(&endtime);

        // Return to the number of digits we want to display
        q--;

        /* print pi */

        printf("pi = %d.",cast(int)(p[0]));
        for (i = 1; i <= q; i++)
        printf("%d",cast(int)(p[i]));
        printf("\n");
        printf("%lld seconds to compute pi with a precision of %d digits.\n", cast(long)(endtime-startime),q);

        return 0;
}

void arctan(int s)
{
        int n;

        t[0] = 1;
        div(s); /* t[] = 1/s */
        add();
        n = 1;
        do {
                mul(n);
                div(s * s);
                div(n += 2);
                if (((n-1) / 2) % 2 == 0)
                        add();
                else
                        sub();
        } while (!tiszero());
}

void add()
{
        int j;

        for (j = q; j >= 0; j--)
        {
                if (t[j] + p[j] > 9) {
                        p[j] += t[j] - 10;
                        p[j-1] += 1;
                } else
                        p[j] += t[j];
        }
}

void sub()
{
        int j;

        for (j = q; j >= 0; j--)
                if (p[j] < t[j]) {
                        p[j] -= t[j] - 10;
                        p[j-1] -= 1;
                } else
                        p[j] -= t[j];
}

void mul(int multiplier)
{
        int b;
        int i;
        int carry = 0, digit = 0;

        for (i = q; i >= 0; i--) {
                b = (t[i] * multiplier + carry);
                digit = b % 10;
                carry = b / 10;
                t[i] = cast(byte)digit;
        }
}

/* t[] /= l */

void div(int divisor)
{
        int i, b;
        int quotient, remainder = 0;

        foreach (ref x; t)
        {
                b = (10 * remainder + x);
                quotient = b / divisor;
                remainder = b % divisor;
                x = cast(byte)quotient;
        }
}

void div4()
{
        int i, c, d = 0;

        for (i = 0; i <= q; i++) {
                c = (10 * d + p[i]) / 4;
                d = (10 * d + p[i]) % 4;
                p[i] = cast(byte)c;
        }
}

void mul4()
{
        int i, c, d;

        d = c = 0;

        for (i = q; i >= 0; i--) {
                d = (p[i] * 4 + c) % 10;
                c = (p[i] * 4 + c) / 10;
                p[i] = cast(byte)d;
        }
}

int tiszero()
{
        int k;

        for (k = 0; k <= q; k++)
                if (t[k] != 0)
                        return false;
        return true;
}
