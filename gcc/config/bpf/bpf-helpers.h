/* Copyright (C) 2019-2023 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* The purpose of this file is to provide a compatiblity layer with
   the Linux kernel bpf_helpers.h header that is located in
   linux/tools/testing/selftests/bpf/bpf_helpers.h.  That file is
   currently llvm-specific.  */

#ifndef __BPF_HELPERS_H
#define __BPF_HELPERS_H

#define SEC(NAME) __attribute__((section(NAME), used))
#define KERNEL_HELPER(NUM) __attribute__((kernel_helper(NUM)))

/* Flags used in some kernel helpers.  */

#define BPF_ANY     0
#define BPF_NOEXIST 1
#define BPF_EXIST   2

#define BPF_F_LOCK 4
#define BPF_F_NO_COMMON_LRU (1U << 1)
#define BPF_F_NUMA_NODE (1U << 2)

/* Prototypes of functions to call kernel helpers.
   Please keep these protoypes sorted by helper number.  */

void *bpf_map_lookup_elem (void *map, const void *key)
  KERNEL_HELPER (1);

int bpf_map_update_elem (void *map, const void *key, const void *value,
			 unsigned long long flags)
  KERNEL_HELPER (2);

int bpf_map_delete_elem (void *map, const void *key)
  KERNEL_HELPER (3);

int bpf_probe_read (void *dst, int size, const void *unsafe_ptr)
  KERNEL_HELPER (4);

unsigned long long bpf_ktime_get_ns (void)
  KERNEL_HELPER (5);

int bpf_trace_printk (const char *fmt, int fmt_size, ...)
  KERNEL_HELPER (6);

unsigned long long bpf_get_prandom_u32 (void)
  KERNEL_HELPER (7);

unsigned long long bpf_get_smp_processor_id (void)
  KERNEL_HELPER (8);

int bpf_skb_store_bytes (void *ctx, int off, void *from, int len,
			 unsigned int start_header)
  KERNEL_HELPER (9);

int bpf_l3_csum_replace (void *ctx, int off, int from, int to, int flags)
  KERNEL_HELPER (10);

int bpf_l4_csum_replace (void *ctx, int off, int from, int to, int flags)
  KERNEL_HELPER (11);

int bpf_tail_call (void *ctx, void *map, unsigned int index)
  KERNEL_HELPER (12);

int bpf_clone_redirect (void *ctx, int ifindex, int flags)
  KERNEL_HELPER (13);

unsigned long long bpf_get_current_pid_tgid (void)
  KERNEL_HELPER (14);

unsigned long long bpf_get_current_uid_gid (void)
  KERNEL_HELPER (15);

int bpf_get_current_comm (void *buf, int buf_size)
  KERNEL_HELPER (16);

unsigned int bpf_get_cgroup_classid (void *ctx)
  KERNEL_HELPER (17);

int bpf_skb_vlan_push (void *ctx, short vlan_proto,
		       unsigned short vlan_tci)
  KERNEL_HELPER (18);

int bpf_skb_vlan_pop (void *ctx)
  KERNEL_HELPER (19);

int bpf_skb_get_tunnel_key (void *ctx, void *key, int size, int flags)
  KERNEL_HELPER (20);

int bpf_skb_set_tunnel_key (void *ctx, void *key, int size, int flags)
  KERNEL_HELPER (21);

unsigned long long bpf_perf_event_read (void *map, unsigned long long flags)
  KERNEL_HELPER (22);

int bpf_redirect (int ifindex, int flags)
  KERNEL_HELPER (23);

unsigned int bpf_get_route_realm (void *ctx)
  KERNEL_HELPER (24);

int bpf_perf_event_output (void *ctx, void *map, unsigned long long flags,
			   void *data, int size)
  KERNEL_HELPER (25);

int bpf_skb_load_bytes (void *ctx, int off, void *to, int len)
  KERNEL_HELPER (26);

int bpf_get_stackid (void *ctx, void *map, int flags)
  KERNEL_HELPER (27);

int bpf_csum_diff (void *from, int from_size, void *to, int to_size, int seed)
  KERNEL_HELPER (28);

int bpf_skb_get_tunnel_opt (void *ctx, void *md, int size)
  KERNEL_HELPER (29);

int bpf_skb_set_tunnel_opt (void *ctx, void *md, int size)
  KERNEL_HELPER (30);

int bpf_skb_change_proto (void *ctx, short proto, unsigned long flags)
  KERNEL_HELPER (31);

int bpf_skb_change_type (void *ctx, unsigned int type)
  KERNEL_HELPER (32);

int bpf_skb_under_cgroup (void *ctx, void *map, int index)
  KERNEL_HELPER (33);

unsigned int bpf_get_hash_recalc (void *ctx)
  KERNEL_HELPER (34);

unsigned long long bpf_get_current_task (void)
  KERNEL_HELPER (35);

int bpf_probe_write_user (void *dst, const void *src, int size)
  KERNEL_HELPER (36);

int bpf_current_task_under_cgroup (void *map, int index)
  KERNEL_HELPER (37);

int bpf_skb_change_tail (void *ctx, unsigned int len, unsigned long flags)
  KERNEL_HELPER (38);

int bpf_skb_pull_data (void *, int len)
  KERNEL_HELPER (39);

long long bpf_csum_update (void *ctx, unsigned int csum)
  KERNEL_HELPER (40);

void bpf_set_hash_invalid (void *ctx)
  KERNEL_HELPER (41);

int bpf_get_numa_node_id (void)
  KERNEL_HELPER (42);

int bpf_skb_change_head (void *, int len, int flags)
  KERNEL_HELPER (43);

int bpf_xdp_adjust_head (void *ctx, int offset)
  KERNEL_HELPER (44);

int bpf_probe_read_str (void *ctx, unsigned int size, const void *unsafe_ptr)
  KERNEL_HELPER (45);

int bpf_get_socket_cookie (void *ctx)
  KERNEL_HELPER (46);

unsigned int bpf_get_socket_uid (void *ctx)
  KERNEL_HELPER (47);

unsigned int bpf_set_hash (void *ctx, unsigned int hash)
  KERNEL_HELPER (48);

int bpf_setsockopt (void *ctx, int level, int optname, void *optval, int optlen)
  KERNEL_HELPER (49);

int bpf_skb_adjust_room (void *ctx, int len_diff, unsigned int mode,
			 unsigned long long flags)
  KERNEL_HELPER (50);

int bpf_redirect_map (void *map, int key, int flags)
  KERNEL_HELPER (51);

int bpf_sk_redirect_map (void *ctx, void *map, int key, int flags)
  KERNEL_HELPER (52);

int bpf_sock_map_update (void *map, void *key, void *value,
			 unsigned long long flags)
  KERNEL_HELPER (53);

int bpf_xdp_adjust_meta (void *ctx, int offset)
  KERNEL_HELPER (54);

int bpf_perf_event_read_value (void *map, unsigned long long flags,
			       void *buf, unsigned int buf_size)
  KERNEL_HELPER (55);

int bpf_perf_prog_read_value (void *ctx, void *buf, unsigned int buf_size)
  KERNEL_HELPER (56);

int bpf_getsockopt (void *ctx, int level, int optname, void *optval,
		    int optlen)
  KERNEL_HELPER (57);

int bpf_override_return (void *ctx, unsigned long rc)
  KERNEL_HELPER (58);

int bpf_sock_ops_cb_flags_set (void *ctx, int flags)
  KERNEL_HELPER (59);

int bpf_msg_redirect_map (void *ctx, void *map, int key, int flags)
  KERNEL_HELPER (60);

int bpf_msg_apply_bytes (void *ctx, int len)
  KERNEL_HELPER (61);

int bpf_msg_cork_bytes (void *ctx, int len)
  KERNEL_HELPER (62);

int bpf_msg_pull_data (void *, int len)
  KERNEL_HELPER (63);

int bpf_bind (void *ctx, void *addr, int addr_len)
  KERNEL_HELPER (64);

int bpf_xdp_adjust_tail (struct xdp_md *xdp_md, int delta)
  KERNEL_HELPER (65);

int bpf_skb_get_xfrm_state (void *ctx, int index, void *state,
			    int size, int flags)
  KERNEL_HELPER (66);

int bpf_get_stack (void *ctx, void *buf, int size, int flags)
  KERNEL_HELPER (67);

int bpf_skb_load_bytes_relative (void *ctx, int off, void *to, int len,
				 unsigned int start_header)
  KERNEL_HELPER (68);

int bpf_fib_lookup (void *ctx, struct bpf_fib_lookup *params,
		    int plen, unsigned int flags)
  KERNEL_HELPER (69);

int bpf_sock_hash_update (void *map, void *key, void *value,
			  unsigned long long flags)
  KERNEL_HELPER (70);

int bpf_msg_redirect_hash (void *ctx, void *map, void *key, int flags)
  KERNEL_HELPER (71);

int bpf_sk_redirect_hash (void *ctx, void *map, void *key, int flags)
  KERNEL_HELPER (72);

int bpf_lwt_push_encap (void *ctx, unsigned int type, void *hdr,
			unsigned int len)
  KERNEL_HELPER (73);

int bpf_lwt_seg6_store_bytes (void *ctx, unsigned int offset,
			      void *from, unsigned int len)
  KERNEL_HELPER (74);

int bpf_lwt_seg6_adjust_srh (void *ctx, unsigned int offset,
			     unsigned int len)
  KERNEL_HELPER (75);

int bpf_lwt_seg6_action (void *ctx, unsigned int action, void *param,
			 unsigned int param_len)
  KERNEL_HELPER (76);

int bpf_rc_repeat (void *ctx)
  KERNEL_HELPER (77);

int bpf_rc_keydown (void *ctx, unsigned int protocol,
		    unsigned long long scancode, unsigned int toggle)
  KERNEL_HELPER (78);

unsigned bpf_skb_cgroup_id (void *ctx)
  KERNEL_HELPER (79);

unsigned long long bpf_get_current_cgroup_id (void)
  KERNEL_HELPER (80);

void *bpf_get_local_storage (void *map, unsigned long long flags)
  KERNEL_HELPER (81);

int bpf_sk_select_reuseport (void *ctx, void *map, void *key, unsigned int flags)
  KERNEL_HELPER (82);

unsigned long long bpf_skb_ancestor_cgroup_id (void *ctx, int level)
  KERNEL_HELPER (83);

struct bpf_sock *bpf_sk_lookup_tcp (void *ctx, struct bpf_sock_tuple *tuple,
				    int size, unsigned long long netns_id,
				    unsigned long long flags)
  KERNEL_HELPER (84);

struct bpf_sock *bpf_sk_lookup_udp (void *ctx, struct bpf_sock_tuple *tuple,
				    int size, unsigned long long netns_id,
				    unsigned long long flags)
  KERNEL_HELPER (85);

int bpf_sk_release (struct bpf_sock *sk)
  KERNEL_HELPER (86);

int bpf_map_push_elem (void *map, const void *value, unsigned long long flags)
  KERNEL_HELPER (87);

int bpf_map_pop_elem (void *map, void *value)
  KERNEL_HELPER (88);

int bpf_map_peek_elem (void *map, void *value)
  KERNEL_HELPER (89);

int bpf_msg_push_data (void *ctx, int start, int cut, int flags)
  KERNEL_HELPER (90);

int bpf_msg_pop_data (void *ctx, int start, int cut, int flags)
  KERNEL_HELPER (91);

int bpf_rc_pointer_rel (void *ctx, int rel_x, int rel_y)
  KERNEL_HELPER (92);

void bpf_spin_lock (struct bpf_spin_lock *lock)
  KERNEL_HELPER (93);

void bpf_spin_unlock (struct bpf_spin_lock *lock)
  KERNEL_HELPER (94);

struct bpf_sock *bpf_sk_fullsock (struct bpf_sock *sk)
  KERNEL_HELPER (95);

struct bpf_sock *bpf_tcp_sock (struct bpf_sock *sk)
  KERNEL_HELPER (96);

int bpf_skb_ecn_set_ce (void *ctx)
  KERNEL_HELPER (97);

struct bpf_sock *bpf_get_listener_sock (struct bpf_sock *sk)
  KERNEL_HELPER (98);

struct bpf_sock *bpf_skc_lookup_tcp (void *ctx,
				     struct bpf_sock_tuple *tuple,
				     unsigned int tuple_size,
				     unsigned long netns,
				     unsigned long flags)
  KERNEL_HELPER (99);

int bpf_tcp_check_syncookie (struct bpf_sock *sk, void *iph,
			     unsigned int iph_len,
			     struct tcp_hdr *th,
			     unsigned int th_len)
  KERNEL_HELPER (100);

int bpf_sysctl_get_name (struct bpf_sysctl *ctx,
			 char *buf, unsigned long buf_len,
			 unsigned long flags)
  KERNEL_HELPER (101);

int bpf_sysctl_get_current_value (struct bpf_sysctl *ctx,
				  char *buf, unsigned long buf_len)
  KERNEL_HELPER (102);

int bpf_sysctl_get_new_value (struct bpf_sysctl *ctx, char *buf,
			      unsigned long buf_len)
  KERNEL_HELPER (103);

int bpf_sysctl_set_new_value (struct bpf_sysctl *ctx, const char *buf,
			      unsigned long buf_len)
  KERNEL_HELPER (104);

int bpf_strtol (const char *buf, unsigned long buf_len,
		unsigned long flags, long *res)
  KERNEL_HELPER (105);

int bpf_strtoul (const char *buf, unsigned long buf_len,
		 unsigned long flags, unsigned long *res)
  KERNEL_HELPER (106);

void *bpf_sk_storage_get (void *map, struct bpf_sock *sk,
			  void *value, long flags)
  KERNEL_HELPER (107);

int bpf_sk_storage_delete (void *map, struct bpf_sock *sk)
  KERNEL_HELPER (108);

/* Functions to emit BPF_LD_ABS and BPF_LD_IND instructions.  We
   provide the "standard" names as synonyms of the corresponding GCC
   builtins.  Note how the SKB argument is ignored.  */

#define load_byte(SKB,OFF) __builtin_bpf_load_byte ((OFF))
#define load_half(SKB,OFF) __builtin_bpf_load_half ((OFF))
#define load_word(SKB,OFF) __builtin_bpf_load_word ((OFF))

struct bpf_map_def
{
  unsigned int type;
  unsigned int key_size;
  unsigned int value_size;
  unsigned int max_entries;
  unsigned int map_flags;
  unsigned int inner_map_idx;
  unsigned int numa_node;
};

#endif /* ! __BPF_HELPERS_H */
