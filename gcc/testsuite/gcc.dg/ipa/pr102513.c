/* { dg-do compile } */
/* { dg-options "-O3 -Warray-bounds" } */

extern int block2[7][256];

static int encode_block(int block2[7][256], unsigned level)
{
    int best_score = 0;

    for (unsigned x = 0; x < level; x++) {
        int v = block2[1][x];
        block2[level][x] = 0;
        best_score += v * v;
    }

    if (level > 0 && best_score > 64) {
        int score = 0;

        score += encode_block(block2, level - 1);
        score += encode_block(block2, level - 1);

        if (score < best_score) {
            best_score = score;
        }
    }

    return best_score;
}

int foo(void)
{
    return encode_block(block2, 5);
}
