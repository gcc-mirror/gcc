/* This used to ICE after PHI-OPT because of the "empty block" and diamond form bbs
   was not checking to make sure each bbs were only coming from the one bb. */

int nilfs_bmap_find_target_seq_bmap;
unsigned long nilfs_bmap_find_target_seq_bmap_0;
unsigned long nilfs_bmap_find_target_seq() {
  if (nilfs_bmap_find_target_seq_bmap &&
      nilfs_bmap_find_target_seq_bmap_0 + nilfs_bmap_find_target_seq_bmap)
    return nilfs_bmap_find_target_seq_bmap_0 + nilfs_bmap_find_target_seq_bmap;
  else
    return 0;
}

