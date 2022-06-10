/* { dg-do compile } */
/* { dg-options "-O2 -std=c++11" } */
#include <array>

static constexpr int NBR_SHIFT = 4;

static constexpr int MAXBOARDSIZE = 25;

static constexpr int MAXSQ = ((MAXBOARDSIZE + 2) * (MAXBOARDSIZE + 2));

enum square_t : char {
        BLACK = 0, WHITE = 1, EMPTY = 2, INVAL = 3
    };

const std::array<int, 2> s_eyemask = {
    4 * (1 << (NBR_SHIFT * BLACK)),
    4 * (1 << (NBR_SHIFT * WHITE))
};

/* counts of neighboring stones */
std::array<unsigned short, MAXSQ> m_neighbours;

int is_eye(const int color, const int i) {
    /* check for 4 neighbors of the same color */
    int ownsurrounded = (m_neighbours[i] & s_eyemask[color]);

    return ownsurrounded;
}

/* { dg-final { scan-assembler "s_eyemask" } } */
