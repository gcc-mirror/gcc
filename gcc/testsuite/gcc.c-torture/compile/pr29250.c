/* We used to ICE because EXPAND_SUM was being used for all recursive calls
   to expand_expr.  */
struct TSparseEntry
{
  int feat_index;
  double entry;
};

struct TSparse
{
  int vec_index;
  int num_feat_entries;
  struct TSparseEntry *features;
};

void
get_full_feature_matrix (struct TSparse* sparse_feature_matrix, int num_vec)
{
  double *fm;
  int v, f;

  for (v=0; v < num_vec; v++)
  {
    for (f=0; f < sparse_feature_matrix[v].num_feat_entries; f++)
    {
      long long offs = sparse_feature_matrix[v].vec_index
	+ sparse_feature_matrix[v].features[f].feat_index;
      fm[offs] = sparse_feature_matrix[v].features[f].entry;
    }
  }
}

