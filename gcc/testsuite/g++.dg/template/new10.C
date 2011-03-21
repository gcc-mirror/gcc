// PR c++/46658

typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
template<class T> class scoped_array {
    void reset(T * p = 0)     { }
};
typedef uint16_t SequenceIndex;
typedef uint32_t SequenceMapIndex;
class Analyzer  {
    template <typename READER>
        bool ReadDictionary( READER& reader );
    scoped_array<SequenceIndex> map_from_2_hints_to_composite_sequence;
    SequenceMapIndex number_of_composite_sequences;
};
template <typename READER>
bool Analyzer::ReadDictionary( READER &reader )
{
  const SequenceMapIndex ntt
    = ( number_of_composite_sequences + SequenceMapIndex( 1 ) )
    * ( number_of_composite_sequences + 1 );
  map_from_2_hints_to_composite_sequence.reset(new SequenceIndex[ntt]());
}
