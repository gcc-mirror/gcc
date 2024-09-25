/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-std=gnu++20 -O2 -march=skylake" } */
/* { dg-final { scan-assembler-not "call\[\\t \]_?memset\[\r\n\]\[^\r\n\]movq\[\\t \]%\[a-z0-9]*, %\[a-z0-9]*\[\r\n\]\[^\r\n\]vpxor\[\\t \]%xmm0, %xmm0, %xmm0\[\r\n\]\[^\r\n\]vmovdqu\[\\t \]%xmm0, 36\\(%rax\\)" } } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */


#include <stdint.h>
#include <vector>
#include <tr1/array>

class FastBoard {
public:
    typedef std::pair<int, int> movescore_t;
    typedef std::tr1::array<movescore_t, 24> scoredlist_t;
    
protected:
    std::vector<int> m_critical;

    int m_boardsize;    
};

class FastState {
public:        
    FastBoard board;
    
    int movenum;              
protected:
    FastBoard::scoredlist_t scoredmoves;
};

class KoState : public FastState {
private:         
    std::vector<uint64_t> ko_hash_history;   
    std::vector<uint64_t> hash_history;     
};

class GameState : public KoState {
public:                    
    void foo ();      
private:
    std::vector<KoState> game_history;                          
};

void GameState::foo() {
    game_history.resize(movenum);
}
