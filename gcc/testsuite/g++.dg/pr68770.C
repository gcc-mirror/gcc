/* { dg-do compile } */
/* { dg-options "-O2" } */
/* This test used to trigger the use of an uninitialised field (t_icode)
   in the secondary_reload_info structure created in ira-costs.c:copy_cost().
   Unfortunately the test only generates a problem if run under valgrind...  */
typedef unsigned long int uint64_t;

namespace __gnu_parallel
{
   enum _AlgorithmStrategy { heuristic, force_sequential, force_parallel };
   enum _SortAlgorithm     { MWMS, QS, QS_BALANCED };
   enum _MultiwayMergeAlgorithm  { LOSER_TREE };
   enum _PartialSumAlgorithm     { RECURSIVE, LINEAR };
   enum _SplittingAlgorithm     { SAMPLING, EXACT };
   enum _FindAlgorithm     { GROWING_BLOCKS, CONSTANT_SIZE_BLOCKS, EQUAL_SPLIT };
   typedef uint64_t _SequenceIndex;
   struct _Settings
   {
     _AlgorithmStrategy algorithm_strategy;
     _SortAlgorithm sort_algorithm;
     _PartialSumAlgorithm partial_sum_algorithm;
     _MultiwayMergeAlgorithm multiway_merge_algorithm;
     _FindAlgorithm find_algorithm;
     _SplittingAlgorithm sort_splitting;
     _SplittingAlgorithm merge_splitting;
     _SplittingAlgorithm multiway_merge_splitting;
     _SequenceIndex accumulate_minimal_n;
     unsigned int adjacent_difference_minimal_n;
     _SequenceIndex count_minimal_n;
     _SequenceIndex fill_minimal_n;
     double find_increasing_factor;
     _SequenceIndex find_initial_block_size;
     _SequenceIndex find_maximum_block_size;
     _SequenceIndex find_sequential_search_size;
     _SequenceIndex for_each_minimal_n;
     _SequenceIndex generate_minimal_n;
     _SequenceIndex max_element_minimal_n;
     _SequenceIndex merge_minimal_n;
     unsigned int merge_oversampling;
     _SequenceIndex min_element_minimal_n;
     _SequenceIndex multiway_merge_minimal_n;
     int multiway_merge_minimal_k;
     unsigned int multiway_merge_oversampling;
     _SequenceIndex nth_element_minimal_n;
     _SequenceIndex partition_chunk_size;
     double partition_chunk_share;
     _SequenceIndex partition_minimal_n;
     _SequenceIndex partial_sort_minimal_n;
     float partial_sum_dilation;
     unsigned int partial_sum_minimal_n;
     float find_scale_factor;

     explicit _Settings() :
       algorithm_strategy(heuristic),
       sort_algorithm(MWMS),
       partial_sum_algorithm(LINEAR),
       multiway_merge_algorithm(LOSER_TREE),
       find_algorithm(CONSTANT_SIZE_BLOCKS),
       sort_splitting(EXACT),
       merge_splitting(EXACT),
       multiway_merge_splitting(EXACT),
       accumulate_minimal_n(1000),
       adjacent_difference_minimal_n(1000),
       count_minimal_n(1000),
       fill_minimal_n(1000),
       find_increasing_factor(2.0),
       find_initial_block_size(256),
       find_maximum_block_size(8192),
       find_sequential_search_size(256),
       for_each_minimal_n(1000),
       generate_minimal_n(1000),
       max_element_minimal_n(1000),
       merge_minimal_n(1000),
       merge_oversampling(10),
       min_element_minimal_n(1000),
       multiway_merge_minimal_n(1000),
       multiway_merge_minimal_k(2),
       multiway_merge_oversampling(10),
       nth_element_minimal_n(1000),
       partition_chunk_size(1000),
       partition_chunk_share(0.0),
       partition_minimal_n(1000),
       partial_sort_minimal_n(1000),
       partial_sum_dilation(1.0f),
       partial_sum_minimal_n(1000),
       find_scale_factor(0.01f)
     { }
   };
}

namespace
{
  __gnu_parallel::_Settings s;
}

