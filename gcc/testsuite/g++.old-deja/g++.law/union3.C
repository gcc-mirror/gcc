// Build don't link: 
// GROUPS passed unions
// anon-union file
// From: dcb@us-es.sel.de (David Binderman 3841)
// Date:     Tue, 30 Mar 93 09:06:15 +0200
// Subject:  Page 183 of the ARM
// Message-ID: <9303300706.AA17079@slsvitt>

static union {
        char*   uC;
private:
        int     uI;// ERROR - 
};
